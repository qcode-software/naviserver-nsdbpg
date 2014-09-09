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
 * tclcmds.c --
 *
 *      Postgres specific Tcl commands and bind variable emulation.
 */

#include "dbpg.h"

/*
 * The following structure defines a linked list of strings
 * for parsing SQL statements.
 */

typedef struct _string_list_elt {
  struct _string_list_elt *next;
  char                    *string;
} string_list_elt_t;


/*
 * Local functions defined in this file.
 */

static Tcl_ObjCmdProc PgObjCmd;
static Tcl_ObjCmdProc PgBindObjCmd;

static int AddCmds(Tcl_Interp *interp, void *arg);
static int DbFail(Tcl_Interp *interp, Ns_DbHandle *handle, char *cmd, char* sql, Ns_DString *dsPtr);
static int BadArgs(Tcl_Interp *interp, Tcl_Obj *CONST argv[], char *args);

static string_list_elt_t *string_list_elt_new(char *string);
static int string_list_len (string_list_elt_t *head);
static void string_list_free_list (string_list_elt_t *head);

static void parse_bind_variables(char *input,
                                 string_list_elt_t **bind_variables,
                                 string_list_elt_t **fragments);
static int blob_get(Tcl_Interp *interp, Ns_DbHandle *handle, char* lob_id);
static int blob_send_to_stream(Tcl_Interp *interp, Ns_DbHandle *handle, char* lob_id,
                               int to_conn_p, char* filename);
static int blob_put(Tcl_Interp *interp, Ns_DbHandle *handle, char* blob_id, char* value);
static int blob_dml_file(Tcl_Interp *interp, Ns_DbHandle *handle, char* blob_id,
                         char* filename);
static int stream_actually_write (int fd, Ns_Conn *conn, void *bufp, int length, int to_conn_p);

static unsigned char enc_one(unsigned char c);
static void encode3(unsigned char *p, char *buf);
static unsigned char get_one(unsigned char c);
static void decode3(unsigned char *p, char *buf, int n);


/*
 *----------------------------------------------------------------------
 *
 * Ns_PgServerInit --
 *
 *      Register a function to add postgres specific commands when an
 *      interp is created.
 *
 * Results:
 *      NS_OK or NS_ERROR.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

int
Ns_PgServerInit(char *server, char *module, char *hDriver)
{
    Ns_TclRegisterTrace(server, AddCmds, NULL, NS_TCL_TRACE_CREATE);
    return NS_OK;
}

static int
AddCmds(Tcl_Interp *interp, void *arg)
{
    Tcl_CreateObjCommand(interp, "ns_pg",   PgObjCmd,  NULL, NULL);
    Tcl_CreateObjCommand(interp, "ns_pg_bind", PgBindObjCmd, NULL, NULL);

    return NS_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * PgObjCmd --
 *
 *      Implements the ns_pg Tcl command.
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on sub-command.
 *
 *----------------------------------------------------------------------
 */

static int
PgObjCmd(ClientData dummy, Tcl_Interp *interp, int argc, Tcl_Obj *CONST argv[])
{
    Ns_DbHandle    *handle;
    Connection     *pconn;
    int            subcmd, result;

    static CONST char *subcmds[] = {
      "blob_write", "blob_get", "blob_put", "blob_dml_file", "blob_select_file", 
      "db", "host", "options", "port", "number", "error", "status", "ntuples",
      NULL
    };

    enum SubCmdIndices {
      BlobWriteIdx, BlobGetIdx, BlobPutIdx, BlobDmlFileIdx, BlobSelectFileIdx,
      DbIdx, HostIdx, OptionsIdx, PortIdx, NumberIdx, ErrorIdx, StatusIdx, NtuplesIdx
    };


    if (Ns_TclDbGetHandle(interp, Tcl_GetString(argv[2]), &handle) != TCL_OK) {
        return TCL_ERROR;
    }

    pconn = handle->connection;

    /*
     * Make sure this is a PostgreSQL handle before accessing
     * handle->connection as an Connection.
     */

    if (Ns_DbDriverName(handle) != pgDbName) {
        Tcl_AppendResult(interp, "handle \"", Tcl_GetString(argv[1]), "\" is not of type \"",
                         pgDbName, "\"", NULL);
        return TCL_ERROR;
    }

    result = Tcl_GetIndexFromObj(interp, argv[1], subcmds, "ns_pg subcmd", 0, &subcmd);
    if (result != TCL_OK) {
        return TCL_ERROR;
    }

    switch (subcmd) {

    case BlobWriteIdx: 
	if (argc != 4) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                             Tcl_GetString(argv[0]), " command dbId blobId\"", NULL);
            return TCL_ERROR;
        }
        return blob_send_to_stream(interp, handle, Tcl_GetString(argv[3]), NS_TRUE, NULL);

    case BlobGetIdx: 
        if (argc != 4) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                             Tcl_GetString(argv[0]), " command dbId blobId\"", NULL);
            return TCL_ERROR;
        }
        return blob_get(interp, handle, Tcl_GetString(argv[3]));

    case BlobPutIdx: 
        if (argc != 5) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                             Tcl_GetString(argv[0]), " command dbId blobId value\"", NULL);
            return TCL_ERROR;
        }
        if (!pconn->in_transaction) {
            Tcl_AppendResult(interp,
                             "blob_put only allowed in transaction", NULL);
            return TCL_ERROR;
        }
        return blob_put(interp, handle, Tcl_GetString(argv[3]), Tcl_GetString(argv[4]));

    case BlobDmlFileIdx:
        if (argc != 5) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                             Tcl_GetString(argv[0]), " command dbId blobId filename\"", NULL);
            return TCL_ERROR;
        }
        if (!pconn->in_transaction) {
            Tcl_AppendResult(interp,
                             "blob_dml_file only allowed in transaction", NULL);
            return TCL_ERROR;
        }
        return blob_dml_file(interp, handle, Tcl_GetString(argv[3]), Tcl_GetString(argv[4]));


    case BlobSelectFileIdx:
        if (argc != 5) {
            Tcl_AppendResult(interp, "wrong # args: should be \"",
                             Tcl_GetString(argv[0]), " command dbId blobId filename\"", NULL);
            return TCL_ERROR;
        }
        return blob_send_to_stream(interp, handle, Tcl_GetString(argv[3]), NS_FALSE, Tcl_GetString(argv[4]));

    case DbIdx:
	if (argc != 3) break;
        Tcl_SetResult(interp, (char *) PQdb(pconn->pgconn), TCL_STATIC);
	return TCL_OK;

    case HostIdx: 
	if (argc != 3) break;
        Tcl_SetResult(interp, (char *) PQhost(pconn->pgconn), TCL_STATIC);
	return TCL_OK;

    case OptionsIdx:
	if (argc != 3) break;
        Tcl_SetResult(interp, (char *) PQoptions(pconn->pgconn), TCL_STATIC);
	return TCL_OK;

    case PortIdx:
	if (argc != 3) break;
        Tcl_SetResult(interp, (char *) PQport(pconn->pgconn), TCL_STATIC);
	return TCL_OK;

    case NumberIdx:
	if (argc != 3) break;
        Tcl_SetObjResult(interp, Tcl_NewIntObj(pconn->id));
	return TCL_OK;
    
    case ErrorIdx:
	if (argc != 3) break;
        Tcl_SetResult(interp, (char *) PQerrorMessage(pconn->pgconn), TCL_STATIC);
	return TCL_OK;

    case StatusIdx:
	if (argc != 3) break;
	Tcl_SetResult(interp, PQstatus(pconn->pgconn) == CONNECTION_OK ? "ok" : "bad", TCL_STATIC);
	return TCL_OK;

    case NtuplesIdx:
	if (argc != 3) break;
        Tcl_SetObjResult(interp, Tcl_NewIntObj(pconn->nTuples));
	return TCL_OK;
    }

    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     Tcl_GetString(argv[0]), " command dbId\"", NULL);
    return TCL_ERROR;
}


/*
 *----------------------------------------------------------------------
 *
 *  ParsedSql Tcl_Obj type --
 *
 *      ParsedSql is an Tcl_Obj type carrying a bind-var-parsed 
 *      SQL statement.
 *
 *----------------------------------------------------------------------
 */

typedef struct {
    string_list_elt_t *sql_fragments;
    string_list_elt_t *bind_variables;
    int nrFragments;
} ParsedSQL;

static Tcl_FreeInternalRepProc	ParsedSQLFreeInternalRep;
static Tcl_SetFromAnyProc       ParsedSQLSetFromAny;
static Tcl_DupInternalRepProc   ParsedSQLDupInternalRep;

Tcl_ObjType ParsedSQLObjType = {
    "parsedSQL",			/* name */
    ParsedSQLFreeInternalRep,		/* freeIntRepProc */
    ParsedSQLDupInternalRep,		/* dupIntRepProc */
    NULL,				/* updateStringProc */
    ParsedSQLSetFromAny			/* setFromAnyProc */
};

/* 
 * freeIntRepProc
 */
static void
ParsedSQLFreeInternalRep(
    register Tcl_Obj *objPtr)	/* parsedSQL Tcl object with internal
				 * representation to free. */
{
  ParsedSQL *parsedSQLptr = (ParsedSQL *)objPtr->internalRep.twoPtrValue.ptr1;

  assert(parsedSQLptr);
  /*fprintf(stderr, "%p ParsedSQLFreeInternalRep freeing ParsedSQL %p refCOunt %d # %d frags %p vars %p\n", 
	  objPtr, 
	  parsedSQLptr, objPtr->refCount,
	  parsedSQLptr->nrFragments,
	  parsedSQLptr->sql_fragments,
	  parsedSQLptr->bind_variables
	  );*/

  if (parsedSQLptr->sql_fragments)  {string_list_free_list(parsedSQLptr->sql_fragments);}
  if (parsedSQLptr->bind_variables) {string_list_free_list(parsedSQLptr->bind_variables);}
  
  /*
   * ... and free structure
   */
  ns_free(parsedSQLptr);
}

/* 
 * dupIntRepProc
 */
static void
ParsedSQLDupInternalRep(
    Tcl_Obj *srcObjPtr,
    Tcl_Obj *dstObjPtr)
{
    ParsedSQL *srcPtr = (ParsedSQL *)srcObjPtr->internalRep.twoPtrValue.ptr1, *dstPtr;

    //fprintf(stderr, "ParsedSQLDupInternalRep src %p dst %p\n", srcObjPtr, dstObjPtr);
  
    dstPtr = ns_calloc(1, sizeof(ParsedSQL));
    if (srcPtr->sql_fragments) {
	//fprintf(stderr, "HAVE TO DUP FRAGMENTS OR TO REGENERATE IT\n");
	dstPtr->sql_fragments = NULL;
    }
    if (srcPtr->bind_variables) {
	dstPtr->bind_variables = NULL;
    }
    dstPtr->nrFragments = srcPtr->nrFragments;
    
    dstObjPtr->typePtr = srcObjPtr->typePtr;
    dstObjPtr->internalRep.twoPtrValue.ptr1 = dstPtr;
}

#define TclFreeIntRep(objPtr) \
    if ((objPtr)->typePtr != NULL && \
            (objPtr)->typePtr->freeIntRepProc != NULL) { \
        (objPtr)->typePtr->freeIntRepProc(objPtr); \
    }

/* 
 * setFromAnyProc
 */
static int
ParsedSQLSetFromAny(
    Tcl_Interp *interp,		/* Used for error reporting if not NULL. */
    register Tcl_Obj *objPtr)	/* The object to convert. */
{
    char      *sql    = Tcl_GetString(objPtr);
    ParsedSQL *srcPtr = ns_calloc(1, sizeof(ParsedSQL));

    /*
     * Parse the query string and find the bind variables.  Return
     * the sql fragments so that the query can be rebuilt with the
     * bind variable values interpolated into the original query.
     */
    parse_bind_variables(sql, &srcPtr->bind_variables, &srcPtr->sql_fragments);
    srcPtr->nrFragments = string_list_len(srcPtr->bind_variables);
    //fprintf(stderr, "ParsedSQLSetFromAny\n");

    /*
     * Free the old interal representation and store own structure as internal
     * representation.
     */
    TclFreeIntRep(objPtr);
    objPtr->internalRep.twoPtrValue.ptr1 = (void *)srcPtr;
    objPtr->internalRep.twoPtrValue.ptr2 = NULL;
    objPtr->typePtr = &ParsedSQLObjType;
    
    return TCL_OK;
}


/*
 *----------------------------------------------------------------------
 *
 * PgBindObjCmd --
 *
 *      Implements the ns_pg_bind command which emulates bind variables.
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      ???
 *
 *----------------------------------------------------------------------
 */

static int
PgBindObjCmd(ClientData dummy, Tcl_Interp *interp, int argc, Tcl_Obj *CONST argv[])
{
    string_list_elt_t *bind_variables;
    string_list_elt_t *var_p;
    string_list_elt_t *sql_fragments;
    string_list_elt_t *frag_p;
    Ns_DString         ds, *dsPtr = NULL;
    Tcl_Obj           *sqlObj;
    ParsedSQL         *parsedSQLptr;
    Ns_DbHandle       *handle;
    Ns_Set            *rowPtr;
    Ns_Set            *set   = NULL;
    char              *cmd, *sql, *value = NULL, *p;
    char              *arg3 = argc > 3 ? Tcl_GetString(argv[3]) : NULL;
    int               haveBind = arg3 ? STREQ("-bind", Tcl_GetString(argv[3])) : 0;
    int               result, subcmd, nrFragments;

    static CONST char *subcmds[] = {
      "dml", "1row", "0or1row", "select", "exec", 
      NULL
    };

    enum SubCmdIndices {
      DmlIdx, OneRowIdx, ZeroOrOneRowIdx, SelectIdx, ExecIdx
    };

    if (argc < 4 
	|| (!haveBind && (argc != 4))
        || (haveBind && (argc != 6))) {
        return BadArgs(interp, argv, "dbId sql");
    }

    result = Tcl_GetIndexFromObj(interp, argv[1], subcmds, "ns_pg_bind subcmd", 0, &subcmd);
    if (result != TCL_OK) {
        return TCL_ERROR;
    }

    if (Ns_TclDbGetHandle(interp, Tcl_GetString(argv[2]), &handle) != TCL_OK) {
        return TCL_ERROR;
    }

    Ns_DStringFree(&handle->dsExceptionMsg);
    handle->cExceptionCode[0] = '\0';

    /*
     * Make sure this is a PostgreSQL handle before accessing
     * handle->connection as an Connection.
     */

    if (Ns_DbDriverName(handle) != pgDbName) {
        Tcl_AppendResult(interp, "handle \"", argv[1], "\" is not of type \"",
                         pgDbName, "\"", NULL);
        return TCL_ERROR;
    }

    cmd = Tcl_GetString(argv[1]);

    if (haveBind) {
	char *setId = Tcl_GetString(argv[4]);
        set = Ns_TclGetSet(interp, setId);
        if (set == NULL) {
            Tcl_AppendResult (interp, "invalid set id `", setId, "'", NULL);
            return TCL_ERROR;
        }
        sqlObj = argv[5];
    } else {
        sqlObj = argv[3];
    }

    if (sqlObj->typePtr != &ParsedSQLObjType) {
	Ns_Log(Debug, "%p convert type %s to sql <%s>", 
	       sqlObj, sqlObj->typePtr ? sqlObj->typePtr->name : "none", Tcl_GetString(sqlObj));
	if (Tcl_ConvertToType(interp, sqlObj, &ParsedSQLObjType) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	Ns_Log(Debug, "%p REUSE sql", sqlObj);
    }
    assert(sqlObj->typePtr == &ParsedSQLObjType);

    sql = Tcl_GetString(sqlObj);

    parsedSQLptr = (ParsedSQL *)sqlObj->internalRep.twoPtrValue.ptr1;
    if (parsedSQLptr->nrFragments > 0 && parsedSQLptr->sql_fragments == NULL) {
	/* 
	 * The obj was a result of a dup operation, we have to
	 * reparse sql_fragments 
	 */
	parse_bind_variables(sql, &parsedSQLptr->bind_variables, &parsedSQLptr->sql_fragments);
	parsedSQLptr->nrFragments = string_list_len(parsedSQLptr->bind_variables);
    }
    bind_variables = parsedSQLptr->bind_variables;
    sql_fragments = parsedSQLptr->sql_fragments;
    nrFragments = parsedSQLptr->nrFragments;

    if (nrFragments > 0) {
	dsPtr = &ds;
        Ns_DStringInit(dsPtr);

        /*
         * Rebuild the query and substitute the actual tcl variable values
         * for the bind variables.
         */

        for (var_p = bind_variables, frag_p = sql_fragments;
             var_p != NULL || frag_p != NULL;) {

            if (frag_p != NULL) {
                Ns_DStringAppend(dsPtr, frag_p->string);
                frag_p = frag_p->next;
            }

            if (var_p != NULL) {
                if (set == NULL) {
                    value = (char*)Tcl_GetVar(interp, var_p->string, 0);
                } else {
                    value = Ns_SetGet(set, var_p->string);
                }
                if (value == NULL) {
                    Tcl_AppendResult (interp, "undefined variable `", var_p->string,
                                      "'", NULL);
                    if (dsPtr) { Ns_DStringFree(dsPtr); }
                    return TCL_ERROR;
                }

                if ( strlen(value) == 0 ) {
                    /*
                     * DRB: If the Tcl variable contains the empty string, pass a NULL
                     * as the value.
                     */
                    Ns_DStringAppend(dsPtr, "NULL");
                } else {
		    int needEscapeStringSyntax = 0;

		    /*
		     * Determine, if we need the SQL escape string syntax E'...'
		     */
		    for (p = value; *p; p++) {
		        if (unlikely(*p == '\\')) {
			    needEscapeStringSyntax = 1;
			    break;
			}
		    }

                    /*
                     * DRB: We really only need to quote strings, but there is one benefit
                     * to quoting numeric values as well.  A value like '35 union select...'
                     * substituted for a legitimate value in a URL to "smuggle" SQL into a
                     * script will cause a string-to-integer conversion error within Postgres.
                     * This conversion is done before optimization of the query, so indices are
                     * still used when appropriate.
                     */
                    Ns_DStringAppend(dsPtr, needEscapeStringSyntax ? "E'" : "'");

                    /*
                     * We need to double-quote quotes and
                     * escape backslashes inside the value.
                     */
                    for (p = value; *p; p++) {
                        if (unlikely(*p == '\'')) {
                            if (likely(p > value)) {
                                Ns_DStringNAppend(dsPtr, value, p-value);
                            }
                            value = p;
                            Ns_DStringNAppend(dsPtr, "'", 1);
                        } else if (unlikely(*p == '\\')) {
                            if (likely(p > value)) {
                                Ns_DStringNAppend(dsPtr, value, p-value);
                            }
                            value = p;
                            Ns_DStringNAppend(dsPtr, "\\", 1);
                        }
                    }

                    if (likely(p > value)) {
			Ns_DStringNAppend(dsPtr, value, p-value);
                    }

                    Ns_DStringNAppend(dsPtr, "'", 1);
                }
                var_p = var_p->next;
            }
        }
    }

    if (dsPtr) {
	sql = Ns_DStringValue(dsPtr);
    }

    switch (subcmd) {
    case DmlIdx:
        if (Ns_DbDML(handle, sql) != NS_OK) {
            return DbFail(interp, handle, cmd, sql, dsPtr);
        }
	break;

    case OneRowIdx:
        rowPtr = Ns_Db1Row(handle, sql);
        if (rowPtr == NULL) {
            return DbFail(interp, handle, cmd, sql, dsPtr);
        }
        Ns_TclEnterSet(interp, rowPtr, 1);
	break;

    case ZeroOrOneRowIdx:
	{
	    int nrows;

	    rowPtr = Ns_Db0or1Row(handle, sql, &nrows);
	    if (rowPtr == NULL) {
		return DbFail(interp, handle, cmd, sql, dsPtr);
	    }
	    if (nrows == 0) {
		Ns_SetFree(rowPtr);
	    } else {
		Ns_TclEnterSet(interp, rowPtr, 1);
	    }
	}
	break;

    case SelectIdx:
        rowPtr = Ns_DbSelect(handle, sql);
        if (rowPtr == NULL) {
            return DbFail(interp, handle, cmd, sql, dsPtr);
        }
        Ns_TclEnterSet(interp, rowPtr, 0);
	break;

    case ExecIdx:
        switch (Ns_DbExec(handle, sql)) {
        case NS_DML:
            Tcl_SetResult(interp, "NS_DML", TCL_STATIC);
            break;
        case NS_ROWS:
            Tcl_SetResult(interp, "NS_ROWS", TCL_STATIC);
            break;
        default:
            return DbFail(interp, handle, cmd, sql, dsPtr);
            break;
        }
	break;
    }

    if (dsPtr) { Ns_DStringFree(dsPtr); }

    return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * DbFail --
 *
 *      Common routine that creates database failure message.
 *
 * Results:
 *      Return TCL_ERROR and set database failure message as Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
DbFail(Tcl_Interp *interp, Ns_DbHandle *handle, char *cmd, char* sql, Ns_DString *dsPtr)
{
    Connection *pconn = handle->connection;
    char       *pqerror;

    Tcl_AppendResult(interp, "Database operation \"", cmd, "\" failed", NULL);
    if (handle->cExceptionCode[0] != '\0') {
        Tcl_AppendResult(interp, " (exception ", handle->cExceptionCode,
                         NULL);
        if (handle->dsExceptionMsg.length > 0) {
            Tcl_AppendResult(interp, ", \"", handle->dsExceptionMsg.string,
                             "\"", NULL);
        }
        Tcl_AppendResult(interp, ")", NULL);
    }

    pqerror = PQerrorMessage(pconn->pgconn);
    if (pqerror[0] != '\0') {
        Tcl_AppendResult(interp, "\n\n", pqerror, NULL);
    } else {
        Tcl_AppendResult(interp, "\n", NULL);
    }
    Tcl_AppendResult(interp, "\nSQL: ", sql, NULL);

    if (dsPtr) { Ns_DStringFree(dsPtr); }

    return TCL_ERROR;
}


/*
 *----------------------------------------------------------------------
 * BadArgs --
 *
 *      Common routine that creates bad arguments message.
 *
 * Results:
 *      Return TCL_ERROR and set bad argument message as Tcl result.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
BadArgs(Tcl_Interp *interp, Tcl_Obj *CONST argv[], char *args)
{
    Tcl_AppendResult(interp, "wrong # args: should be \"",
		     Tcl_GetString(argv[0]), " ", Tcl_GetString(argv[1]), NULL);
    if (args != NULL) {
        Tcl_AppendResult(interp, " ", args, NULL);
    }
    Tcl_AppendResult(interp, "\"", NULL);

    return TCL_ERROR;
}


/*
 *----------------------------------------------------------------------
 *
 * parse_bind_variables --
 *
 *      Parse an SQL string and return a list of all the bind variables
 *      found in it.
 *
 * Results:
 *      The arguments bind_variables and fragments are updated.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static void
parse_bind_variables(char *input,
                     string_list_elt_t **bind_variables,
                     string_list_elt_t **fragments)
{
    char *p, lastchar;
    enum { base, instr, bind } state;
    char *bindbuf, *bp;
    char *fragbuf, *fp;
    string_list_elt_t *elt, *head=0, *tail=0;
    string_list_elt_t *felt, *fhead=0, *ftail=0;
    int current_string_length = 0;

    fragbuf = (char*)ns_malloc((strlen(input)+1)*sizeof(char));
    fp = fragbuf;
    bindbuf = (char*)ns_malloc((strlen(input)+1)*sizeof(char));
    bp = bindbuf;

    for (p = input, state=base, lastchar='\0'; *p != '\0'; lastchar = *p, p++) {

        switch (state) {
        case base:
            if (*p == '\'') {
                state = instr;
                current_string_length = 0;
                *fp++ = *p;
            } else if ((*p == ':') && (*(p + 1) != ':') && (lastchar != ':')) {
                bp = bindbuf;
                state = bind;
                *fp = '\0';
                felt = string_list_elt_new(ns_strdup(fragbuf));
                if(ftail == NULL) {
                    fhead = ftail = felt;
                } else {
                    ftail->next = felt;
                    ftail = felt;
                }
            } else {
                *fp++ = *p;
            }
            break;

        case instr:
            if (*p == '\'' && (lastchar != '\'' || current_string_length == 0)) {
                state = base;
            }
            current_string_length++;
            *fp++ = *p;
            break;

        case bind:
            if (*p == '=') {
                state = base;
                bp = bindbuf;
                fp = fragbuf;
            } else if (!(*p == '_' || *p == '$' || *p == '#' || isalnum((int)*p))) {
                *bp = '\0';
                elt = string_list_elt_new(ns_strdup(bindbuf));
                if (tail == NULL) {
                    head = tail = elt;
                } else {
                    tail->next = elt;
                    tail = elt;
                }
                state = base;
                fp = fragbuf;
                p--;
            } else {
                *bp++ = *p;
            }
            break;
        }
    }

    if (state == bind) {
        *bp = '\0';
        elt = string_list_elt_new(ns_strdup(bindbuf));
        if (tail == NULL) {
            head = tail = elt;
        } else {
            tail->next = elt;
            /*tail = elt;*/
        }
    } else {
        *fp = '\0';
        felt = string_list_elt_new(ns_strdup(fragbuf));
        if (ftail == NULL) {
            fhead = ftail = felt;
        } else {
            ftail->next = felt;
            /*ftail = felt;*/
        }
    }

    ns_free(fragbuf);
    ns_free(bindbuf);
    *bind_variables = head;
    *fragments      = fhead;

    return;
}

/* ns_pg blob_get db blob_id
 * returns the value of the blob to the Tcl caller.
 */

static int
blob_get(Tcl_Interp *interp, Ns_DbHandle *handle, char* lob_id)
{
    Connection *pconn = handle->connection;
    int         segment;
    char        query[100];
    char        *segment_pos;
    /*int         nbytes = 0;*/

    segment = 1;

    strcpy(query, "SELECT BYTE_LEN, DATA FROM LOB_DATA WHERE LOB_ID = ");
    strcat(query, lob_id);
    strcat(query, " AND SEGMENT = ");

    segment_pos = query + strlen(query);

    for (;;) {
        char    *data_column;
        int     i, j, n, byte_len;
        char    buf[6001];

        sprintf(segment_pos, "%d", segment);
        if (Ns_DbExec(handle, query) != NS_ROWS) {
            Tcl_AppendResult(interp, "Error selecting data from BLOB", NULL);
            return TCL_ERROR;
        }

        if (PQntuples(pconn->res) == 0) {
            break;
        }

	byte_len = atoi(PQgetvalue(pconn->res, 0, 0));
        data_column = PQgetvalue(pconn->res, 0, 1);
	/* nbytes is not used 
        nbytes += byte_len;
	*/
        n = byte_len;
        for (i=0, j=0; n > 0; i += 4, j += 3, n -= 3) {
            decode3((unsigned char*)&data_column[i], &buf[j], n);
        }
        buf[byte_len] = '\0';
        Tcl_AppendResult(interp, buf, NULL);
        segment++;
    }

    PQclear(pconn->res);
    pconn->res = NULL;

    return TCL_OK;
}

/* ns_pg blob_select_file db blob_id filename
 * Write a pseudo-blob to the passed in temp file name.  Some of this
 * shamelessly lifted from ora8.c.
 * DanW - This is just blob_write, except it doesn't send anything out the
 *        connection.
 * .
 * Combined blob_select_file and blob_write:
 * If you want to write to the network connection, set TO_CONN_P to NS_TRUE
 * and pass a null filename.
 *
 * If you want to write the blob to a file, set TO_CONN_P = NS_FALSE, and
 * pass the filename in.
 */

static int
blob_send_to_stream(Tcl_Interp *interp, Ns_DbHandle *handle, char* lob_id,
            int to_conn_p, char* filename)
{
    Connection  *pconn = handle->connection;
    Ns_Conn     *conn = NULL;
    int          segment;
    char         query[100];
    int          fd = -1;
    char        *segment_pos;

    if (to_conn_p) {
        conn = Ns_TclGetConn(interp);

        if (conn == NULL) {
            /* this Shouldn't Happen, but spew an error just in case */
            Ns_Log (Error, "blob_send_to_stream: No connection available");
            Tcl_AppendResult (interp, "No connection available", NULL);
            goto bailout;
        }

    } else {
        if (filename == NULL) {
            Tcl_AppendResult (interp, "could not create temporary file to spool "
                              "BLOB/CLOB result", NULL);
            return TCL_ERROR;
        }

        fd = open (filename, O_CREAT | O_TRUNC | O_WRONLY, 0600);

        if (fd < 0) {
            Ns_Log (Error, "Can't open %s for writing. error %d(%s)",
                    filename, errno, strerror(errno));
            Tcl_AppendResult (interp, "can't open file ", filename,
                              " for writing. ",
                              "received error ", strerror(errno), NULL);
            return TCL_ERROR;
        }
    }

    segment = 1;

    strcpy(query, "SELECT BYTE_LEN, DATA FROM LOB_DATA WHERE LOB_ID = ");
    strcat(query, lob_id);
    strcat(query, " AND SEGMENT = ");

    segment_pos = query + strlen(query);

    for (;;) {
        char    *data_column;
        int     i, j, n, byte_len;
        char    buf[6000];

        sprintf(segment_pos, "%d", segment);
        if (Ns_DbExec(handle, query) != NS_ROWS) {
            Tcl_AppendResult(interp, "Error selecting data from BLOB", NULL);
            return TCL_ERROR;
        }
        if (PQntuples(pconn->res) == 0) {
            break;
        }

        byte_len = atoi(PQgetvalue(pconn->res, 0, 0));
        data_column = PQgetvalue(pconn->res, 0, 1);
        n = byte_len;
        for (i=0, j=0; n > 0; i += 4, j += 3, n -= 3) {
            decode3((unsigned char*)&data_column[i], &buf[j], n);
        }

        stream_actually_write(fd, conn, buf, byte_len, to_conn_p);
        segment++;
    }

 bailout:
    if (!to_conn_p) {
        close (fd);
    }

    PQclear(pconn->res);
    pconn->res = NULL;

    return TCL_OK;
}

/**
 * Write the contents of BUFP to a file descriptor or to
 * the network connection directly.
 *
 * Lifted from Oracle driver.
 */
static int
stream_actually_write (int fd, Ns_Conn *conn, void *bufp, int length, int to_conn_p)
{
    int bytes_written = 0;

    if (to_conn_p) {
        int n = Ns_ConnContentSent(conn);

        if (Ns_ConnWriteData(conn, bufp, length, 0) == NS_OK) {
            bytes_written = Ns_ConnContentSent(conn) - n;
        } else {
            bytes_written = 0;
        }
    } else {
        bytes_written = write(fd, bufp, length);
    }

    return bytes_written;
}

/* ns_pg blob_put blob_id value
 * Stuff the contents of value into the pseudo-blob blob_id
 */

static int
blob_put(Tcl_Interp *interp, Ns_DbHandle *handle, char* blob_id, char* value)
{
    int         i, j, segment, value_len;
    char        out_buf[8001], query[10000];
    char        *segment_pos, *value_ptr;

    value_len = strlen(value);
    value_ptr = value;

    strcpy(query, "INSERT INTO LOB_DATA VALUES(");
    strcat(query, blob_id);
    strcat(query, ",");
    segment_pos = query + strlen(query);
    segment = 1;

    while (value_len > 0) {
        int segment_len = value_len > 6000 ? 6000 : value_len;
        value_len -= segment_len;
        for (i = 0, j = 0; i < segment_len; i += 3, j+=4) {
            encode3((unsigned char*)&value_ptr[i], &out_buf[j]);
        }
        out_buf[j] = '\0';
        sprintf(segment_pos, "%d, %d, '%s')", segment, segment_len, out_buf);
        if (Ns_DbExec(handle, query) != NS_DML) {
            Tcl_AppendResult(interp, "Error inserting data into BLOB", NULL);
            return TCL_ERROR;
        }
        value_ptr += segment_len;
        segment++;
    }
    Ns_Log(Notice, "done");

    return TCL_OK;
}

/* ns_pg blob_dml_file blob_id file_name
 * Stuff the contents of file_name into the pseudo-blob blob_id
 */

static int
blob_dml_file(Tcl_Interp *interp, Ns_DbHandle *handle, char* blob_id,
              char* filename)
{
    int         fd, i, j, segment, readlen;
    char        in_buf[6000], out_buf[8001], query[10000];
    char        *segment_pos;

    fd = open (filename, O_RDONLY);

    if (fd == -1) {
        Ns_Log (Error, " Error opening file %s: %d(%s)",
                filename, errno, strerror(errno));
        Tcl_AppendResult (interp, "can't open file ", filename,
                          " for reading. ", "received error ",
                          strerror(errno), NULL);
    }

    strcpy(query, "INSERT INTO LOB_DATA VALUES(");
    strcat(query, blob_id);
    strcat(query, ",");
    segment_pos = query + strlen(query);
    segment = 1;

    readlen = read (fd, in_buf, 6000);
    while (readlen > 0) {
        for (i = 0, j = 0; i < readlen; i += 3, j+=4) {
            encode3((unsigned char*)&in_buf[i], &out_buf[j]);
        }
        out_buf[j] = '\0';
        sprintf(segment_pos, "%d, %d, '%s')", segment, readlen, out_buf);
        if (Ns_DbExec(handle, query) != NS_DML) {
            Tcl_AppendResult(interp, "Error inserting data into BLOB", NULL);
            close(fd);
            return TCL_ERROR;
        }
        readlen = read(fd, in_buf, 6000);
        segment++;
    }
    close(fd);

    return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * string_list_elt_new --
 *
 *      Allocate memory for a new string list element which points to
 *      the given string.
 *
 * Results:
 *      A new string list element.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static string_list_elt_t *
string_list_elt_new(char *string)
{
    string_list_elt_t *elt;

    elt = ns_malloc(sizeof(string_list_elt_t));
    elt->string = string;
    elt->next = NULL;

    return elt;
}


/*
 *----------------------------------------------------------------------
 *
 * string_list_len --
 *
 *      Count the elements in a string list.
 *
 * Results:
 *      Number of elements.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */

static int
string_list_len (string_list_elt_t *head)
{
    int n;

    for (n = 0; head != NULL; head = head->next) {
        n++;
    }
    return n;
}


/*
 *----------------------------------------------------------------------
 *
 * string_list_free_list --
 *
 *      Free the given list and the strings it contains.
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
string_list_free_list (string_list_elt_t *head)
{
    string_list_elt_t *elt;

    while (head != NULL) {
        ns_free(head->string);
        elt = head->next;
        ns_free(head);
        head = elt;
    }
}

/*
 * This is a slight modification of the encoding scheme used by
 * uuencode.  It's quite efficient, using four bytes for each
 * three bytes in the input stream.   There's a slight hitch in
 * that apostrophe isn't legal in Postgres strings.
 * The uuencoding algorithm doesn't make use of lower case letters,
 * though, so we just map them to 'a'.
 *
 * This is a real hack, that's for sure, but we do want to be
 * able to pg_dump these, and this simple means of encoding
 * accomplishes that and is fast, besides.  And at some point
 * we'll be able to stuff large objects directly into Postgres
 * anyway.
 */

/* ENC is the basic 1-character encoding function to make a char printing */
#define ENC(c) (((c) & 077) + ' ')

static unsigned char
enc_one(unsigned char c)
{
    c = ENC(c);
    if (c == '\'') c = 'a';
    else if (c == '\\') c = 'b';
    return c;
}

static void
encode3(unsigned char *p, char *buf)
{
    *buf++ = enc_one(*p >> 2);
    *buf++ = enc_one(((*p << 4) & 060) | ((p[1] >> 4) & 017));
    *buf++ = enc_one(((p[1] << 2) & 074) | ((p[2] >> 6) & 03));
    *buf++ = enc_one(p[2] & 077);
}


/* single-character decode */
#define DEC(c)  (((c) - ' ') & 077)

static unsigned char
get_one(unsigned char c)
{
    if (c == 'a') return '\'';
    else if (c == 'b') return '\\';
    return c;
}

static void
decode3(unsigned char *p, char *buf, int n)
{
    char c1, c2, c3, c4;

    c1 = get_one(p[0]);
    c2 = get_one(p[1]);
    c3 = get_one(p[2]);
    c4 = get_one(p[3]);

    if (n >= 1)
        *buf++ = DEC(c1) << 2 | DEC(c2) >> 4;
    if (n >= 2)
        *buf++ = DEC(c2) << 4 | DEC(c3) >> 2;
    if (n >= 3)
        *buf++ = DEC(c3) << 6 | DEC(c4);
}
