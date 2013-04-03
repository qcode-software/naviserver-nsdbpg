/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://mozilla.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Original Code is AOLserver Code and related documentation
 * distributed by AOL.
 *
 * The Initial Developer of the Original Code is America Online,
 * Inc. Portions created by AOL are Copyright (C) 1999 America Online,
 * Inc. All Rights Reserved.
 *
 * Alternatively, the contents of this file may be used under the terms
 * of the GNU General Public License (the "GPL"), in which case the
 * provisions of GPL are applicable instead of those above.  If you wish
 * to allow use of your version of this file only under the terms of the
 * GPL and not to allow others to use your version of this file under the
 * License, indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by the GPL.
 * If you do not delete the provisions above, a recipient may use your
 * version of this file under either the License or the GPL.
 *
 */

/*
 * nsdbpg.c --
 *
 *      Implements the nsdb driver interface for the postgres database.
 */

#include "dbpg.h"

NS_EXPORT int Ns_ModuleVersion = 1;
char *pgDbName = "PostgreSQL";

/*
 * Local functions defined in this file.
 */

static char   *DbType(Ns_DbHandle *handle);
static int     OpenDb(Ns_DbHandle *handle);
static int     CloseDb(Ns_DbHandle *handle);
static Ns_Set *BindRow(Ns_DbHandle *handle);
static int     Exec(Ns_DbHandle *handle, char *sql);
static int     GetRow(Ns_DbHandle *handle, Ns_Set *row);
static int     GetRowCount(Ns_DbHandle *handle);
static int     Flush(Ns_DbHandle *handle);
static int     ResetHandle(Ns_DbHandle *handle);

static void SetTransactionState(Ns_DbHandle *handle, char *sql);


/*
 * Local variables defined in this file.
 */

static Ns_DbProc procs[] = {
    {DbFn_DbType,       DbType},
    {DbFn_Name,         DbType},
    {DbFn_OpenDb,       OpenDb},
    {DbFn_CloseDb,      CloseDb},
    {DbFn_BindRow,      BindRow},
    {DbFn_Exec,         Exec},
    {DbFn_GetRow,       GetRow},
    {DbFn_GetRowCount,  GetRowCount},
    {DbFn_Flush,        Flush},
    {DbFn_Cancel,       Flush},
    {DbFn_ResetHandle,  ResetHandle},
    {DbFn_ServerInit,   Ns_PgServerInit},
    {0, NULL}
};

static char *dateStyle = NULL;
static unsigned int id = 0;     /* Global count of connections. */


/*
 *----------------------------------------------------------------------
 *
 * Ns_DbDriverInit --
 *
 *      Register driver functions.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

NS_EXPORT int
Ns_DbDriverInit(char *driver, char *configPath)
{
    char       *style;
    Ns_DString  ds;

    if (Ns_DbRegisterDriver(driver, &procs[0]) != NS_OK) {
        return NS_ERROR;
    }

    style = Ns_ConfigGetValue(configPath, "datestyle");
    if (style != NULL) {
        if (STRIEQ(style, "ISO") || STRIEQ(style, "SQL")
            || STRIEQ(style, "POSTGRES") || STRIEQ(style, "GERMAN")
            || STRIEQ(style, "NONEURO") || STRIEQ(style, "EURO")) {

            Ns_DStringInit(&ds);
            Ns_DStringPrintf(&ds, "set datestyle to '%s'", style);
            dateStyle = Ns_DStringExport(&ds);
            Ns_Log(Notice, "nsdbpg: Using datestyle: %s", style);
        } else {
            Ns_Log(Error, "nsdbpg: Illegal value for datestyle: %s", style);
        }
    } else if ((style = getenv("PGDATESTYLE")) != NULL) {
        Ns_Log(Notice, "nsdbpg: PGDATESTYLE in effect: %s", style);
    }

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * DbType --
 *
 *      Return a string which identifies the driver type and name.
 *
 * Results:
 *      Database type/name.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static char *
DbType(Ns_DbHandle *handle)
{
    return pgDbName;
}


/*
 *----------------------------------------------------------------------
 *
 * OpenDb --
 *
 *      Open a connection to a postgres database.  The datasource for
 *      postgres is in the form "host:port:database".
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
OpenDb(Ns_DbHandle *handle)
{
    Connection  *pconn;
    PGconn      *pgconn;
    Ns_DString   ds;
    char        *host, *port, *db;

    if (handle == NULL || handle->datasource == NULL) {
        Ns_Log(Error, "nsdbpg: Invalid handle.");
        return NS_ERROR;
    }

    Ns_DStringInit(&ds);
    Ns_DStringAppend(&ds, handle->datasource);
    host = ds.string;
    port = strchr(host, ':');
    if (port == NULL || ((db = strchr(port + 1, ':')) == NULL)) {
        Ns_Log(Error, "nsdbpg(%s):  Malformed datasource: \" %s\". "
               "Should be host:port:database.",
               handle->driver, handle->datasource);
        return NS_ERROR;
    }

    *port++ = '\0';
    *db++ = '\0';
    if (STREQ(host, "localhost")) {
        Ns_Log(Notice, "nsdbpg: Opening %s on %s", db, host);
        pgconn = PQsetdbLogin(NULL, port, NULL, NULL, db, handle->user,
                             handle->password);
    } else {
        Ns_Log(Notice, "nsdbpg: Opening %s on %s, port %s", db, host, port);
        pgconn = PQsetdbLogin(host, port, NULL, NULL, db, handle->user,
                             handle->password);
    }
    if (PQstatus(pgconn) != CONNECTION_OK) {
        Ns_Log(Error, "nsdbpg(%s):  Could not connect to %s: %s",
               handle->driver, handle->datasource, PQerrorMessage(pgconn));
        PQfinish(pgconn);
        return NS_ERROR;
    }
    Ns_Log(Notice, "nsdbpg(%s):  Openned connection to %s.",
           handle->driver, handle->datasource);

    pconn = ns_malloc(sizeof(Connection));
    pconn->pgconn = pgconn;
    pconn->res = NULL;
    pconn->id = id++;
    pconn->nCols = pconn->nTuples = pconn->curTuple = 0;
    pconn->in_transaction = NS_FALSE;
    handle->connection = pconn;

    if (dateStyle != NULL && Ns_DbExec(handle, dateStyle) != NS_DML) {
        return NS_ERROR;
    }

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * CloseDb --
 *
 *      Close an open connection to postgres.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
CloseDb(Ns_DbHandle *handle) {

    Connection *pconn;

    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: Invalid connection.");
        return NS_ERROR;
    }

    pconn = handle->connection;

    if (handle->verbose) {
        Ns_Log(Notice, "nsdbpg(%s):  Closing connection: %u",
               handle->driver, pconn->id);
    }
    PQfinish(pconn->pgconn);
    ns_free(pconn);
    handle->connection = NULL;

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * BindRow --
 *
 *      Retrieve the column names of the current result.
 *
 * Results:
 *      An Ns_Set whos keys are the names of columns, or NULL on error.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static Ns_Set *
BindRow(Ns_DbHandle *handle)
{
    Connection  *pconn;
    Ns_Set      *row = NULL;

    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: Invalid connection.");
        return NULL;
    }

    if (!handle->fetchingRows) {
        Ns_Log(Error, "nsdbpg(%s): No rows waiting to bind.", handle->datasource);
        return NULL;
    }

    pconn = handle->connection;    
    row = handle->row;

    if (PQresultStatus(pconn->res) == PGRES_TUPLES_OK) {
        int i;

        pconn->curTuple = 0;
        pconn->nCols = PQnfields(pconn->res);
        pconn->nTuples = PQntuples(pconn->res);
        row = handle->row;

        for (i = 0; i < pconn->nCols; i++) {
            Ns_SetPut(row, PQfname(pconn->res, i), NULL);
        }
    }
    handle->fetchingRows = NS_FALSE;

    return row;
}


/*
 *----------------------------------------------------------------------
 *
 * Exec --
 *
 *      Send SQL to the database.
 *
 * Results:
 *      NS_ROWS, NS_DML or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
Exec(Ns_DbHandle *handle, char *sql)
{
    Connection  *pconn;
    Ns_DString   dsSql;
    int          retry_count = 2;
    int          result = NS_ERROR;

    if (sql == NULL) {
        Ns_Log(Error, "nsdbpg: No SQL query.");
        return NS_ERROR;
    }
    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: No connection.");
        return NS_ERROR;
    }

    pconn = handle->connection;
    PQclear(pconn->res);

    Ns_DStringInit(&dsSql);
    Ns_DStringAppend(&dsSql, sql);

    while (dsSql.length > 0 && isspace(dsSql.string[dsSql.length - 1])) {
        dsSql.string[--dsSql.length] = '\0';
    }
    if (dsSql.length > 0 && dsSql.string[dsSql.length - 1] != ';') {
        Ns_DStringAppend(&dsSql, ";");
    }
    if (handle->verbose) {
        Ns_Log(Notice, "nsdbpg: Querying '%s'", dsSql.string);
    }

    pconn->res = PQexec(pconn->pgconn, dsSql.string);

    /* Set error result for exception message -- not sure that this
       belongs here in DRB-improved driver..... but, here it is
       anyway, as it can't really hurt anything :-) */
   
    if (PQresultStatus(pconn->res) == PGRES_BAD_RESPONSE) {
        Ns_DStringAppend(&handle->dsExceptionMsg, "PGRES_BAD_RESPONSE ");
    }
    Ns_DStringAppend(&handle->dsExceptionMsg,
                     PQresultErrorMessage(pconn->res));

    /* This loop should actually be safe enough, but we'll take no 
     * chances and guard against infinite retries with a counter.
     */

    while (PQstatus(pconn->pgconn) == CONNECTION_BAD && retry_count--) {

        int in_transaction = pconn->in_transaction;

        /* Backend returns a fatal error if it really crashed.  After a crash,
         * all other backends close with a non-fatal error because shared
         * memory might've been corrupted by the crash.  In this case, we
         * will retry the query.
         */

        int retry_query = PQresultStatus(pconn->res) == PGRES_NONFATAL_ERROR;

        /* Reconnect messages need to be logged regardless of Verbose. */    

        Ns_Log(Notice, "nsdbpg: Trying to reopen database connection");

        PQfinish(pconn->pgconn);

        /* We'll kick out with an NS_ERROR if we're in a transaction.
         * The queries within the transaction up to this point were
         * rolled back when the transaction crashed or closed itself
         * at the request of the postmaster.  If we were to allow the
         * rest of the transaction to continue, you'd blow transaction
         * semantics, i.e. the first portion of the transaction would've
         * rolled back and the rest of the transaction would finish its
         * inserts or whatever.  Not good!   So we return an error.  If
         * the programmer's catching transaction errors and rolling back
         * properly, there will be no problem - the rollback will be
         * flagged as occuring outside a transaction but there's no
         * harm in that.
         *
         * If the programmer's started a transaction with no "catch",
         * you'll find no sympathy on my part.
         */

        if (OpenDb(handle) == NS_ERROR || in_transaction || !retry_query) {
            if (in_transaction) {
                Ns_Log(Notice, "nsdbpg: In transaction, conn died, error returned");
            }
            Ns_DStringFree(&dsSql);
            return NS_ERROR;
        }

        pconn = handle->connection;

        Ns_Log(Notice, "nsdbpg: Retrying query");
        PQclear(pconn->res);
        pconn->res = PQexec(pconn->pgconn, dsSql.string);

        /* This may again break the connection for two reasons: 
         * our query may be a back-end crashing query or another
         * backend may've crashed after we reopened the backend.
         * Neither's at all likely but we'll loop back and try
         * a couple of times if it does.
         */
    }

    Ns_DStringFree(&dsSql);

    if (pconn->res == NULL) {
        Ns_Log(Error, "nsdbpg(%s):  Could not send query '%s':  %s",
               handle->datasource, sql, PQerrorMessage(pconn->pgconn));
        return NS_ERROR;
    }

    /* DRB: let's clean up pgConn a bit, if someone didn't read all
     * the rows returned by a query, did a dml query, then a getrow
     * the driver might get confused if we don't zap nCols and
     * curTuple.
     */

    pconn->nCols = pconn->nTuples = pconn->curTuple = 0;

    switch(PQresultStatus(pconn->res)) {
    case PGRES_TUPLES_OK:
        handle->fetchingRows = NS_TRUE;
        result = NS_ROWS;
        break;
    case PGRES_COPY_IN:
    case PGRES_COPY_OUT:
        result = NS_DML;
        break;
    case PGRES_COMMAND_OK:
        SetTransactionState(handle, sql);
        sscanf(PQcmdTuples(pconn->res), "%d", &pconn->nTuples);
        result = NS_DML;
        break;
    default:
        Ns_Log(Error, "nsdbpg: result status: %d message: %s",
               PQresultStatus(pconn->res), PQerrorMessage(pconn->pgconn));
        Ns_DbSetException(handle,"ERROR", PQerrorMessage(pconn->pgconn));
        result = NS_ERROR;
    }

    return result;
}


/*
 *----------------------------------------------------------------------
 *
 * GetRow --
 *
 *      Fill in the given Ns_Set with values for each column of the
 *      current row.
 *
 * Results:
 *      NS_OK, NS_END_DATA or NS_ERROR.
 *
 * Side effects:
 *      Current tupple updated.
 *
 *----------------------------------------------------------------------
 */

static int
GetRow(Ns_DbHandle *handle, Ns_Set *row)
{
    Connection  *pconn;
    int          i;

    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: No connection.");
        return NS_ERROR;
    } 

    if (row == NULL) {
        Ns_Log(Error, "nsdbpg: Invalid Ns_Set -> row.");
        return NS_ERROR;
    }

    pconn = handle->connection;

    if (pconn->nCols == 0) {
        Ns_Log(Error, "nsdbpg(%s):  GetRow called outside a fetch row loop.",
               handle->datasource);
        return NS_ERROR;
    }
    if (pconn->curTuple == pconn->nTuples) {
        PQclear(pconn->res);
        pconn->res = NULL;
        pconn->nCols = pconn->nTuples = pconn->curTuple = 0;
        return NS_END_DATA;
    }
    for (i = 0; i < pconn->nCols; i++) {
        Ns_SetPutValue(row, i, PQgetvalue(pconn->res, pconn->curTuple, i));
    }
    pconn->curTuple++;

    return NS_OK;
}

/*
 *----------------------------------------------------------------------
 *
 * GetRowCount --
 *
 *      Returns number of rows processed by the last SQL stetement
 *
 * Results:
 *      Numbe rof rows or NS_ERROR.
 *
 * Side effects:
 *      None
 *
 *----------------------------------------------------------------------
 */

static int
GetRowCount(Ns_DbHandle *handle)
{
    Connection  *pconn;

    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: No connection.");
        return NS_ERROR;
    }
    pconn = handle->connection;

    return pconn->nTuples;
}


/*
 *----------------------------------------------------------------------
 *
 * Flush --
 *
 *      Flush unfetched rows.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
Flush(Ns_DbHandle *handle)
{
    Connection *pconn;

    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: Invalid connection.");
        return NS_ERROR;
    }

    pconn = handle->connection;

    if (pconn->nCols > 0) {
        PQclear(pconn->res);
        pconn->res = NULL;
        pconn->nCols = pconn->nTuples = pconn->curTuple = 0;
    }

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * ResetHandle --
 *
 *      Reset connection ready for next command.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      Any active transaction will be rolled back.
 *
 *----------------------------------------------------------------------
 */

static int
ResetHandle(Ns_DbHandle *handle)
{
    Connection *pconn;

    if (handle == NULL || handle->connection == NULL) {
        Ns_Log(Error, "nsdbpg: Invalid connection.");
        return NS_ERROR;
    }

    pconn = handle->connection;

    if (pconn->in_transaction) {
        if (handle->verbose) {
            Ns_Log(Warning, "nsdbpg: Rolling back transaction.");
        }
        if (Ns_DbExec(handle, "rollback") != PGRES_COMMAND_OK) {
            Ns_Log(Error, "nsdbpg: Rollback failed.");
        }
        return NS_ERROR;
    }

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * SetTransactionState --
 *
 *      Set the current transaction state based on the query pointed
 *      to by "sql".  Should be called only after the query has
 *      successfully been executed.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static void
SetTransactionState(Ns_DbHandle *handle, char *sql)
{
    Connection *pconn = handle->connection;

    while (*sql == ' ') {
        sql++;
    }
    if (!strncasecmp(sql, "begin", 5)) {
        pconn->in_transaction = NS_TRUE;
        if (handle->verbose) {
            Ns_Log(Notice, "nsdbpg: Entering transaction.");
        }
    } else if (!strncasecmp(sql, "end", 3)
               || !strncasecmp(sql, "commit", 6)) {
        pconn->in_transaction = NS_FALSE;
        if (handle->verbose) {
            Ns_Log(Notice, "nsdbpg: Committing transaction.");
        }
    } else if (!strncasecmp(sql, "abort", 5)
               || !strncasecmp(sql, "rollback", 8)) {
        pconn->in_transaction = NS_FALSE;
        if (handle->verbose) {
            Ns_Log(Notice, "nsdbpg: Rolling back transaction.");
        }
    }
}
