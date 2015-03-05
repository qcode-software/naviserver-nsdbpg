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

typedef struct linkedListElement_t {
    struct linkedListElement_t *next;
    const char                 *chars;
} linkedListElement_t;


/*
 * Local functions defined in this file.
 */

static Tcl_ObjCmdProc PgObjCmd;
static Tcl_ObjCmdProc PgBindObjCmd;
static Ns_TclTraceProc AddCmds;

static int DbFail(Tcl_Interp *interp, Ns_DbHandle *handle, const char *cmd, const char *sql)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static int BadArgs(Tcl_Interp *interp, Tcl_Obj *CONST argv[], const char *args)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static linkedListElement_t *linkedListElement_new(const char *chars)
    NS_GNUC_NONNULL(1)
    NS_GNUC_RETURNS_NONNULL;

static int LinkedList_len(const linkedListElement_t *head);

static void LinkedList_free_list (linkedListElement_t *head);

static void parse_bind_variables(const char *input,
                                 linkedListElement_t **bind_variables,
                                 linkedListElement_t **fragments)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static int blob_get(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static int blob_send_to_stream(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id,
                               int to_conn_p, const char *filename)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static int blob_put(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, const char *value)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static int blob_dml_file(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, const char *filename)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static ssize_t write_to_stream(int fd, Ns_Conn *conn, const void *bufp, size_t length, int to_conn_p)
    NS_GNUC_NONNULL(3);

static unsigned char enc_one(unsigned char c);
static unsigned char get_one(unsigned char c);

static void encode3(const unsigned char *p, unsigned char *buf)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2);

static void decode3(const unsigned char *p, char *buf, int n)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2);

static int get_blob_tuples(Tcl_Interp *interp, Ns_DbHandle *handle, char *query, Ns_Conn  *conn, int fd) 
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);





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
Ns_PgServerInit(const char *server, char *UNUSED(module), char *UNUSED(driver))
{
    return Ns_TclRegisterTrace(server, AddCmds, NULL, NS_TCL_TRACE_CREATE);
}

static int
AddCmds(Tcl_Interp *interp, const void *UNUSED(arg))
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
PgObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int argc, Tcl_Obj *CONST argv[])
{
    Ns_DbHandle    *handle;
    Connection     *pconn;
    int             subcmd, result;

    static const char *subcmds[] = {
	"blob_write", "blob_get", "blob_put", "blob_dml_file", "blob_select_file", 
	"db", "host", "options", "port", "number", "error", "status", "ntuples",
	NULL
    };

    enum SubCmdIndices {
	BlobWriteIdx, BlobGetIdx, BlobPutIdx, BlobDmlFileIdx, BlobSelectFileIdx,
	DbIdx, HostIdx, OptionsIdx, PortIdx, NumberIdx, ErrorIdx, StatusIdx, NtuplesIdx
    };

    if (argc < 3) {
        Tcl_WrongNumArgs(interp, 1, argv, "subcmd handle ?args?");
        return TCL_ERROR;
    }

    if (Ns_TclDbGetHandle(interp, Tcl_GetString(argv[2]), &handle) != TCL_OK) {
        return TCL_ERROR;
    }

    assert(handle != NULL);
    pconn = handle->connection;

    /*
     * Make sure this is a PostgreSQL handle before accessing
     * handle->connection as an Connection.
     */

    if (Ns_DbDriverName(handle) != pgDbName) {
        Ns_TclPrintfResult(interp, "handle '%s' is not of type '%s'", Tcl_GetString(argv[1]), pgDbName);
        return TCL_ERROR;
    }

    result = Tcl_GetIndexFromObj(interp, argv[1], subcmds, "ns_pg subcmd", 0, &subcmd);
    if (result != TCL_OK) {
        return TCL_ERROR;
    }

    switch (subcmd) {

    case BlobWriteIdx: 
        if (argc == 4) {
            result = blob_send_to_stream(interp, handle, Tcl_GetString(argv[3]), NS_TRUE, NULL);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle blobId");
            result = TCL_ERROR;
        }
        break;

    case BlobGetIdx: 
        if (argc == 4) {
            result = blob_get(interp, handle, Tcl_GetString(argv[3]));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle blobId");
            result = TCL_ERROR;
        }
        break;

    case BlobPutIdx: 
        if (argc == 5) {
            if (pconn->in_transaction == 0) {
                Ns_TclPrintfResult(interp, "blob_put only allowed in transaction");
                result = TCL_ERROR;
            } else {
                result = blob_put(interp, handle, Tcl_GetString(argv[3]), Tcl_GetString(argv[4]));
            }
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle blobId value");
            result = TCL_ERROR;
        }
        break;

    case BlobDmlFileIdx:
        if (argc == 5) {
            if (pconn->in_transaction == 0) {
                Ns_TclPrintfResult(interp, "blob_dml_file only allowed in transaction");
                result = TCL_ERROR;
            } else {
                result = blob_dml_file(interp, handle, Tcl_GetString(argv[3]), Tcl_GetString(argv[4]));
            }
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle blobId filename");
            result = TCL_ERROR;
        }
        break;

    case BlobSelectFileIdx:
        if (argc == 5) {
            result = blob_send_to_stream(interp, handle, Tcl_GetString(argv[3]), NS_FALSE, Tcl_GetString(argv[4]));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle blobId filename");
            result = TCL_ERROR;
        }
        break;

    case DbIdx:
        if (argc == 3) {
            Tcl_SetResult(interp, PQdb(pconn->pgconn), TCL_STATIC);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    case HostIdx: 
        if (argc == 3) {
            Tcl_SetResult(interp, PQhost(pconn->pgconn), TCL_STATIC);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    case OptionsIdx:
        if (argc == 3) {
            Tcl_SetResult(interp, PQoptions(pconn->pgconn), TCL_STATIC);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    case PortIdx:
        if (argc == 3) {
            Tcl_SetResult(interp, PQport(pconn->pgconn), TCL_STATIC);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    case NumberIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewIntObj((int)pconn->id));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;
    
    case ErrorIdx:
        if (argc == 3) {
            Tcl_SetResult(interp, PQerrorMessage(pconn->pgconn), TCL_STATIC);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    case StatusIdx:
        if (argc == 3) {
            Tcl_SetResult(interp, PQstatus(pconn->pgconn) == CONNECTION_OK ? "ok" : "bad", TCL_STATIC);
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    case NtuplesIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewIntObj(pconn->nTuples));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
	break;

    default:
	/* should not happen */
	assert(subcmd && 0);
        break;
    }

    return result;
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
    linkedListElement_t *sql_fragments;
    linkedListElement_t *bind_variables;
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
ParsedSQLFreeInternalRep(register Tcl_Obj *objPtr)	/* parsedSQL Tcl object with internal
							 * representation to free. */
{
    ParsedSQL *parsedSQLptr = (ParsedSQL *)objPtr->internalRep.twoPtrValue.ptr1;

    assert(parsedSQLptr != NULL);
    /*fprintf(stderr, "%p ParsedSQLFreeInternalRep freeing ParsedSQL %p refCOunt %d # %d frags %p vars %p\n", 
      objPtr, 
      parsedSQLptr, objPtr->refCount,
      parsedSQLptr->nrFragments,
      parsedSQLptr->sql_fragments,
      parsedSQLptr->bind_variables
      );*/

    if (parsedSQLptr->sql_fragments != NULL)  {LinkedList_free_list(parsedSQLptr->sql_fragments);}
    if (parsedSQLptr->bind_variables != NULL) {LinkedList_free_list(parsedSQLptr->bind_variables);}
  
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

    dstPtr = ns_calloc(1U, sizeof(ParsedSQL));
    if (srcPtr->sql_fragments != NULL) {
	dstPtr->sql_fragments = NULL;
    }
    if (srcPtr->bind_variables != NULL) {
	dstPtr->bind_variables = NULL;
    }
    dstPtr->nrFragments = srcPtr->nrFragments;
    
    dstObjPtr->typePtr = srcObjPtr->typePtr;
    dstObjPtr->internalRep.twoPtrValue.ptr1 = dstPtr;
}

#define TclFreeIntRep(objPtr)				\
    if ((objPtr)->typePtr != NULL &&			\
	(objPtr)->typePtr->freeIntRepProc != NULL) {	\
        (objPtr)->typePtr->freeIntRepProc(objPtr);	\
    }

/* 
 * setFromAnyProc
 */
static int
ParsedSQLSetFromAny(Tcl_Interp *UNUSED(interp),
		    register Tcl_Obj *objPtr)	/* The object to convert. */
{
    const char *sql    = Tcl_GetString(objPtr);
    ParsedSQL  *srcPtr = ns_calloc(1U, sizeof(ParsedSQL));

    /*
     * Parse the query string and find the bind variables.  Return
     * the sql fragments so that the query can be rebuilt with the
     * bind variable values interpolated into the original query.
     */
    parse_bind_variables(sql, &srcPtr->bind_variables, &srcPtr->sql_fragments);
    srcPtr->nrFragments = LinkedList_len(srcPtr->bind_variables);

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
PgBindObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int argc, Tcl_Obj *CONST argv[])
{
    linkedListElement_t *bind_variables;
    linkedListElement_t *var_p;
    linkedListElement_t *sql_fragments;
    linkedListElement_t *frag_p;
    Ns_DString         ds, *dsPtr = NULL;
    Tcl_Obj           *sqlObj;
    ParsedSQL         *parsedSQLptr;
    Ns_DbHandle       *handle;
    Ns_Set            *rowPtr, *set = NULL;
    const char        *sql, *cmd, *p, *value = NULL;
    const char        *arg3 = (argc > 3) ? Tcl_GetString(argv[3]) : NULL;
    int                result, subcmd, nrFragments;
    int                haveBind = (arg3 != NULL) ? STREQ("-bind", arg3) : 0;

    static const char *subcmds[] = {
	"dml", "1row", "0or1row", "select", "exec", 
	NULL
    };

    enum SubCmdIndices {
	DmlIdx, OneRowIdx, ZeroOrOneRowIdx, SelectIdx, ExecIdx
    };

    if (argc < 4 
	|| (haveBind == 0 && (argc != 4))
        || (haveBind != 0 && (argc != 6))) {
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

    if (haveBind != 0) {
	const char *setId = Tcl_GetString(argv[4]);
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
	       (void *)sqlObj, 
	       (sqlObj->typePtr != NULL) ? sqlObj->typePtr->name : "none", 
	       Tcl_GetString(sqlObj));
	if (Tcl_ConvertToType(interp, sqlObj, &ParsedSQLObjType) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else {
	Ns_Log(Debug, "%p REUSE sql", (void *)sqlObj);
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
	parsedSQLptr->nrFragments = LinkedList_len(parsedSQLptr->bind_variables);
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
                Ns_DStringAppend(dsPtr, frag_p->chars);
                frag_p = frag_p->next;
            }

            if (var_p != NULL) {
                if (set == NULL) {
                    value = Tcl_GetVar(interp, var_p->chars, 0);
                } else {
                    value = Ns_SetGet(set, var_p->chars);
                }
                if (value == NULL) {
                    Tcl_AppendResult (interp, "undefined variable `", var_p->chars,
                                      "'", NULL);
                    if (dsPtr != NULL) { Ns_DStringFree(dsPtr); }
                    return TCL_ERROR;
                }

                if ( strlen(value) == 0u ) {
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
                    Ns_DStringAppend(dsPtr, (needEscapeStringSyntax != 0) ? "E'" : "'");

                    /*
                     * We need to double-quote quotes and
                     * escape backslashes inside the value.
                     */
                    for (p = value; *p; p++) {
                        if (unlikely(*p == '\'')) {
                            if (likely(p > value)) {
                                Ns_DStringNAppend(dsPtr, value, p - value);
                            }
                            value = p;
                            Ns_DStringNAppend(dsPtr, "'", 1);
                        } else if (unlikely(*p == '\\')) {
                            if (likely(p > value)) {
                                Ns_DStringNAppend(dsPtr, value, p - value);
                            }
                            value = p;
                            Ns_DStringNAppend(dsPtr, "\\", 1);
                        }
                    }

                    if (likely(p > value)) {
			Ns_DStringNAppend(dsPtr, value, p - value);
                    }

                    Ns_DStringNAppend(dsPtr, "'", 1);
                }
                var_p = var_p->next;
            }
        }
    }

    if (dsPtr != NULL) {
	sql = Ns_DStringValue(dsPtr);
    }

    result = TCL_OK;
    switch (subcmd) {
    case DmlIdx:
        if (Ns_DbDML(handle, sql) != NS_OK) {
            result = DbFail(interp, handle, cmd, sql);
        }
	break;

    case OneRowIdx:
        rowPtr = Ns_Db1Row(handle, sql);
        if (rowPtr == NULL) {
            result = DbFail(interp, handle, cmd, sql);
        } else {
            (void)Ns_TclEnterSet(interp, rowPtr, NS_TCL_SET_DYNAMIC);
        }
	break;

    case ZeroOrOneRowIdx:
	{
	    int nrows;

	    rowPtr = Ns_Db0or1Row(handle, sql, &nrows);
	    if (rowPtr == NULL) {
		result = DbFail(interp, handle, cmd, sql);
	    } else {
                if (nrows == 0) {
                    Ns_SetFree(rowPtr);
                } else {
                    (void)Ns_TclEnterSet(interp, rowPtr, NS_TCL_SET_DYNAMIC);
                }
            }
	}
	break;

    case SelectIdx:
        rowPtr = Ns_DbSelect(handle, sql);
        if (rowPtr == NULL) {
            result = DbFail(interp, handle, cmd, sql);
        } else {
            (void)Ns_TclEnterSet(interp, rowPtr, NS_TCL_SET_STATIC);
        }
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
            result = DbFail(interp, handle, cmd, sql);
        }
	break;

    default:
	/* should not happen */
	assert(subcmd && 0);
        break;
    }

    if (dsPtr != NULL) {
        Ns_DStringFree(dsPtr);
    }
    return result;
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
DbFail(Tcl_Interp *interp, Ns_DbHandle *handle, const char *cmd, const char *sql)
{
    Connection *pconn = handle->connection;
    const char *pqerror;

    assert(interp != NULL);
    assert(handle != NULL);
    assert(cmd != NULL);
    assert(sql != NULL);

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
BadArgs(Tcl_Interp *interp, Tcl_Obj *CONST argv[], const char *args)
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
parse_bind_variables(const char *input,
                     linkedListElement_t **bind_variables,
                     linkedListElement_t **fragments)
{
    enum { base, instr, bind } state;
    const char  *p;
    char        *bindbuf, *bp, *fragbuf, *fp, lastchar;
    int          current_string_length = 0;
    size_t       inputLen;
    linkedListElement_t *elt,  *head=0,  *tail=0;
    linkedListElement_t *felt, *fhead=0, *ftail=0;

    assert(input != NULL);
    assert(bind_variables != NULL);
    assert(fragments != NULL);

    inputLen = strlen(input);
    fragbuf = ns_malloc((inputLen + 1U) * sizeof(char));
    fp = fragbuf;
    bindbuf = ns_malloc((inputLen + 1U) * sizeof(char));
    bp = bindbuf;

    for (p = input, state=base, lastchar='\0'; *p != '\0'; lastchar = *p, p++) {

        switch (state) {
        case base:
            if (unlikely(*p == '\'')) {
                state = instr;
                current_string_length = 0;
                *fp++ = *p;
            } else if ((*p == ':') && (*(p + 1) != ':') && (lastchar != ':')) {
                bp = bindbuf;
                state = bind;
                *fp = '\0';
                felt = linkedListElement_new(ns_strdup(fragbuf));
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
            if ((unlikely(*p == '\'')) && (lastchar != '\'' || current_string_length == 0)) {
                state = base;
            }
            current_string_length++;
            *fp++ = *p;
            break;

        case bind:
            if (unlikely(*p == '=')) {
                state = base;
                bp = bindbuf;
                fp = fragbuf;
            } else if (!(*p == '_' || *p == '$' || *p == '#' || CHARTYPE(alnum, *p) != 0)) {
                *bp = '\0';
                elt = linkedListElement_new(ns_strdup(bindbuf));
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

        default:
            /* should not happen */
            assert(state && 0);
        }
    }

    if (state == bind) {
        *bp = '\0';
        elt = linkedListElement_new(ns_strdup(bindbuf));
        if (tail == NULL) {
            head = elt;
            /*tail = elt;*/
        } else {
            tail->next = elt;
            /*tail = elt;*/
        }
    } else {
        *fp = '\0';
        felt = linkedListElement_new(ns_strdup(fragbuf));
        if (ftail == NULL) {
            fhead = felt;
	    /*  ftail = felt; */
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
get_blob_tuples(Tcl_Interp *interp, Ns_DbHandle *handle, char *query, Ns_Conn  *conn, int fd) 
{
    Connection *pconn = handle->connection;
    char       *segment_pos;
    int         segment = 1;

    assert(interp != NULL);
    assert(handle != NULL);
    assert(query != NULL);

    segment_pos = query + strlen(query);

    for (;;) {
	const char *data_column;
	int         i, j, n, byte_len;
	char        buf[6001] = "";

	sprintf(segment_pos, "%d", segment);
	if (Ns_DbExec(handle, query) != NS_ROWS) {
	    Tcl_AppendResult(interp, "Error selecting data from BLOB", NULL);
	    return TCL_ERROR;
	}

	if (PQntuples(pconn->res) == 0) {
	    break;
	}

	byte_len = strtol(PQgetvalue(pconn->res, 0, 0), NULL, 10);
	data_column = PQgetvalue(pconn->res, 0, 1);
	/* 
	   nbytes is not used 
	   nbytes += byte_len;
	*/
	n = byte_len;
	for (i = 0, j = 0; n > 0; i += 4, j += 3, n -= 3) {
	    decode3((const unsigned char*)&data_column[i], &buf[j], n);
	}

	if (fd != NS_INVALID_FD || conn != NULL) {
	    (void) write_to_stream(fd, conn, buf, (size_t)byte_len, (conn != NULL) ? 1 : 0);
	} else {
	    buf[byte_len] = '\0';
	    Tcl_AppendResult(interp, buf, NULL);
	}
	segment++;
    }

    return TCL_OK;
}


static int
blob_get(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id)
{
    Connection *pconn = handle->connection;
    char        query[100];
    int         result;

    assert(interp != NULL);
    assert(handle != NULL);
    assert(lob_id != NULL);

    strcpy(query, "SELECT BYTE_LEN, DATA FROM LOB_DATA WHERE LOB_ID = ");
    strcat(query, lob_id);
    strcat(query, " AND SEGMENT = ");

    result = get_blob_tuples(interp, handle, query, NULL, 0); 

    PQclear(pconn->res);
    pconn->res = NULL;

    return result;
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
blob_send_to_stream(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id,
		    int to_conn_p, const char *filename)
{
    Connection  *pconn = handle->connection;
    Ns_Conn     *conn = NULL;
    char         query[100];
    int          fd = -1, result = TCL_OK;

    assert(interp != NULL);
    assert(handle != NULL);
    assert(lob_id != NULL);

    if (to_conn_p != 0) {
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

        fd = ns_open(filename, O_CREAT | O_TRUNC | O_WRONLY, 0600);

        if (fd < 0) {
            Ns_Log (Error, "Can't open %s for writing. error %d(%s)",
                    filename, errno, strerror(errno));
            Tcl_AppendResult (interp, "can't open file ", filename,
                              " for writing. ",
                              "received error ", strerror(errno), NULL);
            return TCL_ERROR;
        }
    }

    strcpy(query, "SELECT BYTE_LEN, DATA FROM LOB_DATA WHERE LOB_ID = ");
    strcat(query, lob_id);
    strcat(query, " AND SEGMENT = ");

    result = get_blob_tuples(interp, handle, query, conn, fd); 

 bailout:
    if (to_conn_p == 0) {
        (void) ns_close(fd);
    }

    PQclear(pconn->res);
    pconn->res = NULL;

    return result;
}

/**
 * Write the contents of BUFP to a file descriptor or to
 * the network connection directly.
 *
 * Lifted from Oracle driver.
 */
static ssize_t
write_to_stream(int fd, Ns_Conn *conn, const void *bufp, size_t length, int to_conn_p)
{
    ssize_t bytes_written = 0;

    assert(bufp != NULL);
    assert(fd != NS_INVALID_FD || conn != NULL);

    if (to_conn_p != 0) {
        size_t n = Ns_ConnContentSent(conn);

        if (Ns_ConnWriteData(conn, bufp, length, 0u) == NS_OK) {
            bytes_written = (ssize_t)Ns_ConnContentSent(conn) - (ssize_t)n;
        } else {
            bytes_written = 0;
        }
    } else {
	bytes_written = ns_write(fd, bufp, length);
    }

    return bytes_written;
}

/* ns_pg blob_put blob_id value
 * Stuff the contents of value into the pseudo-blob blob_id
 */

static int
blob_put(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, const char *value)
{
    int            i, j, segment, value_len;
    unsigned char  out_buf[8001];
    const unsigned char  *value_ptr;
    char           query[10000];
    char          *segment_pos;

    assert(interp != NULL);
    assert(handle != NULL);
    assert(blob_id != NULL);
    assert(value != NULL);

    value_len = (int)strlen(value);
    value_ptr = (const unsigned char*)value;

    strcpy(query, "INSERT INTO LOB_DATA VALUES(");
    strcat(query, blob_id);
    strcat(query, ",");
    segment_pos = query + strlen(query);
    segment = 1;

    while (value_len > 0) {
        int segment_len = value_len > 6000 ? 6000 : value_len;
        value_len -= segment_len;
        for (i = 0, j = 0; i < segment_len; i += 3, j+=4) {
            encode3(&value_ptr[i], &out_buf[j]);
        }
        out_buf[j] = UCHAR('\0');
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
blob_dml_file(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, const char *filename)
{
    int           fd, i, j, segment, readlen;
    char          query[10000];
    unsigned char in_buf[6000], out_buf[8001];
    char         *segment_pos;

    assert(interp != NULL);
    assert(handle != NULL);
    assert(blob_id != NULL);
    assert(filename != NULL);

    fd = ns_open(filename, O_RDONLY, 0);

    if (fd == NS_INVALID_FD) {
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

    readlen = ns_read(fd, in_buf, 6000u);
    while (readlen > 0) {
        for (i = 0, j = 0; i < readlen; i += 3, j+=4) {
	    encode3(&in_buf[i], &out_buf[j]);
        }
        out_buf[j] = UCHAR('\0');
        sprintf(segment_pos, "%d, %d, '%s')", segment, readlen, out_buf);
        if (Ns_DbExec(handle, query) != NS_DML) {
            Tcl_AppendResult(interp, "Error inserting data into BLOB", NULL);
            (void) ns_close(fd);
            return TCL_ERROR;
        }
        readlen = ns_read(fd, in_buf, 6000u);
        segment++;
    }
    (void) ns_close(fd);

    return TCL_OK;
}



/*
 *----------------------------------------------------------------------
 *
 * linkedListElement_new --
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

static linkedListElement_t *
linkedListElement_new(const char *chars)
{
    linkedListElement_t *elt;

    assert(chars != NULL);

    elt = ns_malloc(sizeof(linkedListElement_t));
    elt->chars = chars;
    elt->next = NULL;

    return elt;
}


/*
 *----------------------------------------------------------------------
 *
 * LinkedList_len --
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
LinkedList_len(const linkedListElement_t *head)
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
 * LinkedList_free_list --
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
LinkedList_free_list (linkedListElement_t *head)
{
    linkedListElement_t *elt;

    while (head != NULL) {
        ns_free((char *)head->chars);
        elt = head->next;
        ns_free(head);
        head = elt;
    }
}

/*
 * encode3() is a slight modification of the encoding scheme used by uuencode.
 * It's quite efficient, using four bytes for each three bytes in the input
 * stream.  There's a slight hitch in that apostrophe isn't legal in Postgres
 * strings.  The uuencoding algorithm doesn't make use of lower case letters,
 * though, so we just map them to 'a'.
 *
 * This is a real hack, that's for sure, but we do want to be able to pg_dump
 * these, and this simple means of encoding accomplishes that and is fast,
 * besides.  And at some point we'll be able to stuff large objects directly
 * into Postgres anyway.
 */

/* ENC is the basic 1-character encoding function to make a char printing */
/*#define ENC(c) (((unsigned char)(c) & 0x3Fu) + (unsigned char)' ')*/

static unsigned char
enc_one(unsigned char c)
{
    c = (c & 0x3Fu) + UCHAR(' ');
    if (c == UCHAR('\'')) {
	c = UCHAR('a');
    } else if (c == UCHAR('\\')) {
	c = UCHAR('b');
    }
    return c;
}

static void
encode3(const unsigned char *p, unsigned char *buf)
{
    *buf++ = enc_one(*p >> 2);
    *buf++ = enc_one((UCHAR(*p << 4)   & 0x30u) | ((p[1] >> 4) & 0x0Fu));
    *buf++ = enc_one((UCHAR(p[1] << 2) & 0x3Cu) | ((p[2] >> 6) & 0x03u));
    *buf++ = enc_one(p[2] & 0x3Fu);
}


/* single-character decode */
#define DEC(c)  (((c) - UCHAR(' ')) & 0x3Fu)

static unsigned char
get_one(unsigned char c)
{
    if (c == UCHAR('a')) {
	return UCHAR('\'');
    } else if (c == UCHAR('b')) {
	return UCHAR('\\');
    }
    return c;
}

static void
decode3(const unsigned char *p, char *buf, int n)
{
    unsigned char c1, c2, c3, c4;

    assert(p != NULL);
    assert(buf != NULL);

    c1 = get_one(p[0]);
    c2 = get_one(p[1]);
    c3 = get_one(p[2]);
    c4 = get_one(p[3]);

    if (n >= 1) {
        *buf++ = UCHAR(DEC(c1) << 2) | DEC(c2) >> 4;
    }
    if (n >= 2) {
        *buf++ = UCHAR(DEC(c2) << 4) | DEC(c3) >> 2;
    }
    if (n >= 3) {
        *buf++ = UCHAR(DEC(c3) << 6) | DEC(c4);
    }
}

/*
 * Local Variables:
 * mode: c
 * c-basic-offset: 4
 * fill-column: 78
 * indent-tabs-mode: nil
 * End:
 */
