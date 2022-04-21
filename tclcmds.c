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
    int                         length;
} linkedListElement_t;


/*
 * Local functions defined in this file.
 */

static Tcl_ObjCmdProc PgObjCmd;
static Tcl_ObjCmdProc PgBindObjCmd;
static Tcl_ObjCmdProc PgBindDmlObjCmd;
static Tcl_ObjCmdProc PgBindOneRowObjCmd;
static Tcl_ObjCmdProc PgBindZeroOrOneRowObjCmd;
static Tcl_ObjCmdProc PgBindSelectObjCmd;
static Tcl_ObjCmdProc PgBindExecObjCmd;

static Ns_TclTraceProc AddCmds;

static const Tcl_ObjType *intTypePtr = NULL;

static int DbFail(Tcl_Interp *interp, Ns_DbHandle *handle, const char *cmd, const char *sql)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static linkedListElement_t *linkedListElement_new(const char *chars, int length)
    NS_GNUC_NONNULL(1)
    NS_GNUC_RETURNS_NONNULL;

static int LinkedList_len(const linkedListElement_t *head);
static void LinkedList_free_list (linkedListElement_t *head);

static linkedListElement_t *
ListElementExternal(const char *msg, char *chars, int len, Tcl_DString *encDsPtr)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(4)
    NS_GNUC_RETURNS_NONNULL;

static const char *SqlObjToString(Tcl_Interp *interp, Ns_Set *bindSet, Tcl_Obj *sqlObj, Tcl_DString *dsPtr)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static void parse_bind_variables(const char *input,
                                 linkedListElement_t **bind_variables,
                                 linkedListElement_t **fragments)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static int blob_get(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static int blob_send_to_stream(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id,
                               bool to_conn_p, const char *filename)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3);

static int blob_put(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, Tcl_Obj *valueObj)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static int blob_dml_file(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, const char *filename)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2) NS_GNUC_NONNULL(3) NS_GNUC_NONNULL(4);

static ssize_t write_to_stream(int fd, Ns_Conn *conn, const void *bufp, size_t length, bool to_conn_p)
    NS_GNUC_NONNULL(3);

static unsigned char enc_one(unsigned char c);
static unsigned char get_one(unsigned char c);

static void encode3(const unsigned char *p, unsigned char *buf)
    NS_GNUC_NONNULL(1) NS_GNUC_NONNULL(2);

static void decode3(const unsigned char *p, unsigned char *buf, long n)
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

Ns_ReturnCode
Ns_PgServerInit(const char *server, const char *UNUSED(module), const char *UNUSED(driver))
{
    intTypePtr = Tcl_GetObjType("int");
    if (intTypePtr == NULL) {
        Tcl_Panic("NsTclInitObjs: no int type");
    }
    return Ns_TclRegisterTrace(server, AddCmds, NULL, NS_TCL_TRACE_CREATE);
}

static Ns_ReturnCode
AddCmds(Tcl_Interp *interp, const void *UNUSED(arg))
{
    (void)Tcl_CreateObjCommand(interp, "ns_pg",      PgObjCmd,     NULL, NULL);
    (void)Tcl_CreateObjCommand(interp, "ns_pg_bind", PgBindObjCmd, NULL, NULL);

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
PgObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int argc, Tcl_Obj *const argv[])
{
    Ns_DbHandle      *handle;
    const Connection *pconn;
    int               subcmd, result;

    static const char *const subcmds[] = {
        "blob_write", "blob_get", "blob_put", "blob_dml_file", "blob_select_file",
        "db", "host", "options", "port", "number", "error", "status", "ntuples",
        "pid",
        NULL
    };

    enum SubCmdIndices {
        BlobWriteIdx, BlobGetIdx, BlobPutIdx, BlobDmlFileIdx, BlobSelectFileIdx,
        DbIdx, HostIdx, OptionsIdx, PortIdx, NumberIdx, ErrorIdx, StatusIdx, NtuplesIdx,
        PidIdx
    };

    if (argc > 1) {
        result = Tcl_GetIndexFromObj(interp, argv[1], subcmds, "ns_pg subcmd", 0, &subcmd);
        if (result != TCL_OK) {
            return TCL_ERROR;
        }
    }

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


    switch (subcmd) {

    case PidIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewIntObj(PQbackendPID(pconn->pgconn)));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
        break;

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
            if (!pconn->in_transaction) {
                Ns_TclPrintfResult(interp, "blob_put only allowed in transaction");
                result = TCL_ERROR;
            } else {
                result = blob_put(interp, handle, Tcl_GetString(argv[3]), argv[4]);
            }
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle blobId value");
            result = TCL_ERROR;
        }
        break;

    case BlobDmlFileIdx:
        if (argc == 5) {
            if (!pconn->in_transaction) {
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
            Tcl_SetObjResult(interp, Tcl_NewStringObj(PQdb(pconn->pgconn), -1));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
        break;

    case HostIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewStringObj(PQhost(pconn->pgconn), -1));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
        break;

    case OptionsIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewStringObj(PQoptions(pconn->pgconn), -1));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
        break;

    case PortIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewStringObj(PQport(pconn->pgconn), -1));
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
            Tcl_SetObjResult(interp, Tcl_NewStringObj(PQerrorMessage(pconn->pgconn), -1));
        } else {
            Tcl_WrongNumArgs(interp, 2, argv, "handle");
            result = TCL_ERROR;
        }
        break;

    case StatusIdx:
        if (argc == 3) {
            Tcl_SetObjResult(interp, Tcl_NewStringObj(PQstatus(pconn->pgconn) == CONNECTION_OK
                                                      ? "ok"
                                                      : "bad", -1));
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
 *      ParsedSql is a Tcl_Obj type carrying a bind-var-parsed
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

static Tcl_ObjType ParsedSQLObjType = {
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

    if (parsedSQLptr->sql_fragments != NULL)  {
        LinkedList_free_list(parsedSQLptr->sql_fragments);
    }
    if (parsedSQLptr->bind_variables != NULL) {
        LinkedList_free_list(parsedSQLptr->bind_variables);
    }

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
    const ParsedSQL *srcPtr = (const ParsedSQL *)srcObjPtr->internalRep.twoPtrValue.ptr1;
    ParsedSQL       *dstPtr;

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
     * the SQL fragments so that the query can be rebuilt with the
     * bind variable values interpolated into the original query.
     */
    parse_bind_variables(sql, &srcPtr->bind_variables, &srcPtr->sql_fragments);
    srcPtr->nrFragments = LinkedList_len(srcPtr->bind_variables);

    /*
     * Free the old internal representation and store own structure as internal
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
 * ListElementExternal --
 *
 *      Return a ListElement with the content converted to external
 *      (UTF-8).  The last argument is used for termporary storage.
 *
 * Results:
 *      None.
 *
 * Side effects:
 *      Updates dsPtr content.
 *
 *----------------------------------------------------------------------
 */
static linkedListElement_t *
ListElementExternal(const char *msg, char *chars, int len, Tcl_DString *encDsPtr)
{
    int encodedLength;

    NS_NONNULL_ASSERT(chars != NULL);
    NS_NONNULL_ASSERT(encDsPtr != NULL);

    (void)*msg;
    (void) Tcl_UtfToExternalDString(NULL, chars, len, encDsPtr);
    // Ns_Log(Notice, "FRAGBUF %s len %d <%s>", msg, encDsPtr->length, encDsPtr->string);
    encodedLength = encDsPtr->length;
    return linkedListElement_new(Ns_DStringExport(encDsPtr), encodedLength);
}


/*
 *----------------------------------------------------------------------
 *
 * SqlObjToString --
 *
 *      Return a string (SQL command) with substituted bind variables in
 *      external encoding. When the command returns NULL, an error message is
 *      set in the interpreter result.
 *
 * Results:
 *      Non-NULL SQL or NULL on failure.
 *
 * Side effects:
 *      Might convert the Tcl_Obj type of sqlObj.
 *
 *----------------------------------------------------------------------
 */

static const char *
SqlObjToString(Tcl_Interp *interp, Ns_Set *bindSet, Tcl_Obj *sqlObj, Tcl_DString *dsPtr)
{
    ParsedSQL                 *parsedSQLptr;
    const linkedListElement_t *bind_variables, *sql_fragments;
    int                        nrFragments;
    const char                *sql;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(sqlObj != NULL);
    NS_NONNULL_ASSERT(dsPtr != NULL);

    if (sqlObj->typePtr != &ParsedSQLObjType) {
        Ns_Log(Debug, "%p convert type %s to sql <%s>",
               (void *)sqlObj,
               (sqlObj->typePtr != NULL) ? sqlObj->typePtr->name : "none",
               Tcl_GetString(sqlObj));
        if (Tcl_ConvertToType(interp, sqlObj, &ParsedSQLObjType) != TCL_OK) {
            Ns_TclPrintfResult(interp,
                               "provided SQL string cannot be converted to SQL type");
            return NULL;
        }
    } else {
        Ns_Log(Debug, "%p REUSE sql", (void *)sqlObj);
    }
    assert(sqlObj->typePtr == &ParsedSQLObjType);

    parsedSQLptr = (ParsedSQL *)sqlObj->internalRep.twoPtrValue.ptr1;
    if (parsedSQLptr->nrFragments > 0 && parsedSQLptr->sql_fragments == NULL) {
        /*
         * The Tcl_Obj was a result of a dup operation, we have to reparse
         * sql_fragments.
         */
        parse_bind_variables(Tcl_GetString(sqlObj),
                             &parsedSQLptr->bind_variables,
                             &parsedSQLptr->sql_fragments);
        parsedSQLptr->nrFragments = LinkedList_len(parsedSQLptr->bind_variables);
    }
    bind_variables = parsedSQLptr->bind_variables;
    sql_fragments = parsedSQLptr->sql_fragments;
    nrFragments = parsedSQLptr->nrFragments;

    if (nrFragments == 0) {
        sql = sql_fragments ? sql_fragments->chars :  Tcl_GetString(sqlObj);
        Ns_Log(Debug, "SQL without fragments <%s>", sql);

    } else {
        const linkedListElement_t *var_p, *frag_p;
        Ns_ReturnCode              result = NS_OK;

        /*
         * Rebuild the query and substitute the actual Tcl variable values
         * for the bind variables.
         */

        for (var_p = bind_variables, frag_p = sql_fragments;
             var_p != NULL || frag_p != NULL;
             ) {
            const char *p;
            char       *value = NULL;
            int         valueLength = -1;

            if (frag_p != NULL) {
                /*
                 * The values in the fragments are already encoded.
                 */
                Ns_DStringNAppend(dsPtr, frag_p->chars, frag_p->length);
                Ns_Log(Debug, "... appended encoded fragment <%s>", frag_p->chars);
                frag_p = frag_p->next;
            }

            if (var_p != NULL) {
                if (bindSet == NULL) {
                    /*
                     * The bind values have to be obtained directly from the
                     * calling environment.
                     */
                    Tcl_Obj *valueObj;

                    valueObj = Tcl_GetVar2Ex(interp, var_p->chars, NULL, 0);
                    if (unlikely(valueObj == NULL)) {
                        value = NULL;
                    } else {
                        value = Tcl_GetStringFromObj(valueObj, &valueLength);

                        if (valueObj->typePtr == intTypePtr) {
                            /*
                             * Since we can trust the byterep, we can bypass
                             * the costly string analysis and
                             * Tcl_UtfToExternalDString check.
                             */
                            Tcl_DStringAppend(dsPtr, "'", 1);
                            Tcl_DStringAppend(dsPtr, value, valueLength);
                            Tcl_DStringAppend(dsPtr, "'", 1);
                            /*fprintf(stderr, "bypass conversion for '%s'\n", value);*/

                            var_p = var_p->next;
                            continue;

                            /*} else if (valueObj->typePtr != NULL) {
                              fprintf(stderr, "can i bypeass conversion for '%s' with type %s?\n",
                              var_p->chars, valueObj->typePtr->name);*/
                        }
                    }

                } else {
                    /*
                     * The bind values are provided explicitly via an ns_set.
                     */
                    value = (char *)Ns_SetGet(bindSet, var_p->chars);
                    valueLength = strlen(value);
                }

                if (value == NULL) {
                    Ns_TclPrintfResult(interp, "undefined variable '%s'",
                                       var_p->chars);
                    result = NS_ERROR;
                    break;

                } else if ( *value == '\0' ) {
                    /*
                     * If the bind value is just an empty string, pass "NULL"
                     * as the value for SQL.
                     */
                    Ns_DStringNAppend(dsPtr, "NULL", 4);
                    var_p = var_p->next;
                    continue;

                } else {
                    Tcl_DString  encDs, *encDsPtr = &encDs;
                    const char  *encodedString;
                    int          encodedLength;

                    Tcl_UtfToExternalDString(NULL, value, valueLength, encDsPtr);
                    encodedLength = encDsPtr->length;
                    encodedString = encDsPtr->string;
                    if (strlen(encodedString) < (size_t)encodedLength) {
                        Ns_TclPrintfResult(interp,
                                           "bind var '%s' contains NUL character",
                                           var_p->chars);
                        Tcl_DStringFree(encDsPtr);
                        result = NS_ERROR;
                        break;
                    }

                    /*
                     * Determine, if we need the SQL escape string syntax
                     * E'...'. In both cases, we open the quotation of the
                     * string here.
                     */
                    if (likely(strchr(encodedString, INTCHAR('\\')) == NULL)) {
                        Ns_DStringNAppend(dsPtr, "'", 1);
                    } else {
                        Ns_DStringNAppend(dsPtr, "E'", 2);
                    }

                    /*
                     * We really only need to quote strings, but there is one
                     * benefit to quoting numeric values as well.  A value
                     * like '35 union select...'  substituted for a legitimate
                     * value in a URL to "smuggle" SQL into a script will
                     * cause a string-to-integer conversion error within
                     * Postgres.  This conversion is done before optimization
                     * of the query, so indices are still used when
                     * appropriate.
                     *
                     * We need to double-quote quotes and escape backslashes
                     * inside the value.
                     */
                    for (p = encodedString; *p != '\0'; p++) {
                        if (unlikely(*p == '\'')) {
                            if (likely(p > encodedString)) {
                                Ns_DStringNAppend(dsPtr, encodedString, (int)(p - encodedString));
                            }
                            encodedString = p;
                            Ns_DStringNAppend(dsPtr, "'", 1);
                        } else if (unlikely(*p == '\\')) {
                            if (likely(p > encodedString)) {
                                Ns_DStringNAppend(dsPtr, encodedString, (int)(p - encodedString));
                            }
                            encodedString = p;
                            Ns_DStringNAppend(dsPtr, "\\", 1);
                        }
                    }

                    if (likely(p > encodedString)) {
                        Ns_DStringNAppend(dsPtr, encodedString, (int)(p - encodedString));
                    }
                    /*
                     * Terminate the quoted value.
                     */
                    Ns_DStringNAppend(dsPtr, "'", 1);

                    Tcl_DStringFree(encDsPtr);
                }
                var_p = var_p->next;
            }
        }
        if (result == NS_OK) {
            sql = Ns_DStringValue(dsPtr);
        } else {
            sql = NULL;
        }
    }
    //Ns_Log(Notice, "final SQL <%s>", sql);

    return sql;
}


/*
 *----------------------------------------------------------------------
 *
 * PgBindDmlObjCmd --
 *
 *      Implements "ns_pg_bind dml /handle/ -bind /bind/ /sql/".
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on subcommand.
 *
 *----------------------------------------------------------------------
 */
static int
PgBindDmlObjCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const* objv)
{
    Ns_Set         *bindSet = NULL;
    Tcl_Obj        *sqlObj;
    int             result = TCL_OK;
    Ns_ObjvSpec     lopts[] = {
        {"-bind", Ns_ObjvSet, &bindSet, NULL},
        {NULL, NULL, NULL, NULL}
    };
    Ns_ObjvSpec     args[] = {
        {"sql",    Ns_ObjvObj, &sqlObj, NULL},
        {NULL, NULL, NULL, NULL}
    };

    if (Ns_ParseObjv(lopts, args, interp, 3, objc, objv) != NS_OK) {
        result = TCL_ERROR;

    } else {
        const char  *sql;
        Tcl_DString  ds;
        Ns_DbHandle *handle = (Ns_DbHandle *)clientData;

        Tcl_DStringInit(&ds);
        sql = SqlObjToString(interp, bindSet, sqlObj, &ds);
        if (sql != NULL) {
            if (Ns_DbDML(handle, sql) != NS_OK) {
                result = DbFail(interp, handle, "dml", sql);
            }
        } else {
            result = TCL_ERROR;
        }
        Tcl_DStringFree(&ds);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PgBindOneRowObjCmd --
 *
 *      Implements "ns_pg_bind 1row /handle/ -bind /bind/ /sql/".
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on subcommand.
 *
 *----------------------------------------------------------------------
 */
static int
PgBindOneRowObjCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const* objv)
{
    Ns_Set         *bindSet = NULL;
    Tcl_Obj        *sqlObj;
    int             result = TCL_OK;
    Ns_ObjvSpec     lopts[] = {
        {"-bind", Ns_ObjvSet, &bindSet, NULL},
        {NULL, NULL, NULL, NULL}
    };
    Ns_ObjvSpec     args[] = {
        {"sql",    Ns_ObjvObj, &sqlObj, NULL},
        {NULL, NULL, NULL, NULL}
    };

    if (Ns_ParseObjv(lopts, args, interp, 3, objc, objv) != NS_OK) {
        result = TCL_ERROR;

    } else {
        const char  *sql;
        Tcl_DString  ds;
        Ns_DbHandle *handle = (Ns_DbHandle *)clientData;

        Tcl_DStringInit(&ds);
        sql = SqlObjToString(interp, bindSet, sqlObj, &ds);
        if (sql != NULL) {
            Ns_Set *rowPtr = Ns_Db1Row(handle, sql);

            if (rowPtr == NULL) {
                result = DbFail(interp, handle, "1row", sql);
            } else {
                (void)Ns_TclEnterSet(interp, rowPtr, NS_TCL_SET_DYNAMIC);
            }
        } else {
            result = TCL_ERROR;
        }
        Tcl_DStringFree(&ds);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PgBindZeroOrOneRowObjCmd --
 *
 *      Implements "ns_pg_bind 0or1row /handle/ -bind /bind/ /sql/".
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on subcommand.
 *
 *----------------------------------------------------------------------
 */
static int
PgBindZeroOrOneRowObjCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const* objv)
{
    Ns_Set         *bindSet = NULL;
    Tcl_Obj        *sqlObj;
    int             result = TCL_OK;
    Ns_ObjvSpec     lopts[] = {
        {"-bind", Ns_ObjvSet, &bindSet, NULL},
        {NULL, NULL, NULL, NULL}
    };
    Ns_ObjvSpec     args[] = {
        {"sql",    Ns_ObjvObj, &sqlObj, NULL},
        {NULL, NULL, NULL, NULL}
    };

    if (Ns_ParseObjv(lopts, args, interp, 3, objc, objv) != NS_OK) {
        result = TCL_ERROR;

    } else {
        const char  *sql;
        Tcl_DString  ds;
        Ns_DbHandle *handle = (Ns_DbHandle *)clientData;

        Tcl_DStringInit(&ds);
        sql = SqlObjToString(interp, bindSet, sqlObj, &ds);
        if (sql != NULL) {
            int     nrows;
            Ns_Set *rowPtr = Ns_Db0or1Row(handle, sql, &nrows);

            if (rowPtr == NULL) {
                result = DbFail(interp, handle, "0or1row", sql);
            } else {
                if (nrows == 0) {
                    Ns_SetFree(rowPtr);
                } else {
                    (void)Ns_TclEnterSet(interp, rowPtr, NS_TCL_SET_DYNAMIC);
                }
            }
        } else {
            result = TCL_ERROR;

        }
        Tcl_DStringFree(&ds);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PgBindSelectObjCmd --
 *
 *      Implements "ns_pg_bind select /handle/ -bind /bind/ /sql/".
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on subcommand.
 *
 *----------------------------------------------------------------------
 */
static int
PgBindSelectObjCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const* objv)
{
    Ns_Set         *bindSet = NULL;
    Tcl_Obj        *sqlObj;
    int             result = TCL_OK;
    Ns_ObjvSpec     lopts[] = {
        {"-bind", Ns_ObjvSet, &bindSet, NULL},
        {NULL, NULL, NULL, NULL}
    };
    Ns_ObjvSpec     args[] = {
        {"sql",    Ns_ObjvObj, &sqlObj, NULL},
        {NULL, NULL, NULL, NULL}
    };

    if (Ns_ParseObjv(lopts, args, interp, 3, objc, objv) != NS_OK) {
        result = TCL_ERROR;

    } else {
        const char  *sql;
        Tcl_DString  ds;
        Ns_DbHandle *handle = (Ns_DbHandle *)clientData;

        Tcl_DStringInit(&ds);
        sql = SqlObjToString(interp, bindSet, sqlObj, &ds);
        if (sql != NULL) {
            Ns_Set *rowPtr = Ns_DbSelect(handle, sql);

            if (rowPtr == NULL) {
                result = DbFail(interp, handle, "select", sql);
            } else {
                (void)Ns_TclEnterSet(interp, rowPtr, NS_TCL_SET_STATIC);
            }
        } else {
            result = TCL_ERROR;
        }
        Tcl_DStringFree(&ds);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PgBindExecObjCmd --
 *
 *      Implements "ns_pg_bind exec /handle/ -bind /bind/ /sql/".
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on subcommand.
 *
 *----------------------------------------------------------------------
 */
static int
PgBindExecObjCmd(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *const* objv)
{
    Ns_Set         *bindSet = NULL;
    Tcl_Obj        *sqlObj;
    int             result = TCL_OK;
    Ns_ObjvSpec     lopts[] = {
        {"-bind", Ns_ObjvSet, &bindSet, NULL},
        {NULL, NULL, NULL, NULL}
    };
    Ns_ObjvSpec     args[] = {
        {"sql",    Ns_ObjvObj, &sqlObj, NULL},
        {NULL, NULL, NULL, NULL}
    };

    if (Ns_ParseObjv(lopts, args, interp, 3, objc, objv) != NS_OK) {
        result = TCL_ERROR;

    } else {
        const char  *sql;
        Tcl_DString  ds;
        Ns_DbHandle *handle = (Ns_DbHandle *)clientData;

        Tcl_DStringInit(&ds);
        sql = SqlObjToString(interp, bindSet, sqlObj, &ds);
        if (sql != NULL) {
            switch (Ns_DbExec(handle, sql)) {
            case NS_DML:
                Tcl_SetObjResult(interp, Tcl_NewStringObj("NS_DML", 6));
                break;
            case NS_ROWS:
                Tcl_SetObjResult(interp, Tcl_NewStringObj("NS_ROWS", 7));
                break;
            default:
                result = DbFail(interp, handle, "exec", sql);
            }
        } else {
            result = TCL_ERROR;
        }
        Tcl_DStringFree(&ds);
    }
    return result;
}

/*
 *----------------------------------------------------------------------
 *
 * PgBindObjCmd --
 *
 *      Implements "ns_pg_bind", a variant of the "ns_db" API for
 *      evaluating PostgreSQL statements, supporting bind variables
 *      (denoted in SQL statements starting with a ":"). The values for
 *      the bind variables are either from the ns_set provided via the
 *      "-bind" option or from the calling environment.
 *
 * Results:
 *      A standard Tcl result.
 *
 * Side effects:
 *      Depends on subcommand.
 *
 *----------------------------------------------------------------------
 */

int
PgBindObjCmd(ClientData UNUSED(clientData), Tcl_Interp *interp, int objc, Tcl_Obj *const* objv)
{
    char        *handleString = (char*)NS_EMPTY_STRING;
    Ns_DbHandle *handle;
    int          nargs, result;
    Tcl_Obj     *subCmdObj;
    Ns_ObjvSpec  args[] = {
        {"subcmd", Ns_ObjvObj,    &subCmdObj, NULL},
        {"handle", Ns_ObjvString, &handleString, NULL},
        {"args",   Ns_ObjvArgs,   &nargs,  NULL},
        {NULL, NULL, NULL, NULL}
    };

    const Ns_SubCmdSpec subcmds[] = {
        {"dml",     PgBindDmlObjCmd},
        {"1row",    PgBindOneRowObjCmd},
        {"0or1row", PgBindZeroOrOneRowObjCmd},
        {"select",  PgBindSelectObjCmd},
        {"exec",    PgBindExecObjCmd},
        {NULL, NULL}
    };

    if (Ns_ParseObjv(NULL, args, interp, 1, objc, objv) != NS_OK) {
        result = TCL_ERROR;

    } else if (Ns_TclDbGetHandle(interp, handleString, &handle) != TCL_OK) {
        result = TCL_ERROR;

    } else if (Ns_DbDriverName(handle) != pgDbName) {
        /*
         * It is no PostgreSQL handle.
         */
        Ns_TclPrintfResult(interp, "handle \"%s\" is not of type \"%s\"",
                           handleString, pgDbName);
        result = TCL_ERROR;

    } else {

        Ns_DStringFree(&handle->dsExceptionMsg);
        handle->cExceptionCode[0] = '\0';
        result = Ns_SubcmdObjv(subcmds, (ClientData)handle, interp, objc, objv);
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
    const Connection *pconn;
    const char       *pqerror;
    Tcl_DString       ds;
    size_t            maxLen = 10000;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(handle != NULL);
    NS_NONNULL_ASSERT(cmd != NULL);
    NS_NONNULL_ASSERT(sql != NULL);

    Tcl_DStringInit(&ds);
    pconn = handle->connection;

    Ns_DStringPrintf(&ds, "Database operation \"%s\" failed", cmd);
    if (handle->cExceptionCode[0] != '\0') {
        Ns_DStringPrintf(&ds, " (exception %s", handle->cExceptionCode);
        if (handle->dsExceptionMsg.length > 0) {
            Ns_DStringPrintf(&ds, ", \"%s\"", handle->dsExceptionMsg.string);
        }
        Ns_DStringPrintf(&ds, ")");
    }

    pqerror = PQerrorMessage(pconn->pgconn);
    if (pqerror[0] != '\0') {
        Ns_DStringPrintf(&ds, "\n\n%s", pqerror);
    } else {
        Ns_DStringPrintf(&ds, "\n");
    }
    if (handle->verbose) {
        if (strlen(sql) > maxLen) {
            Ns_DStringPrintf(&ds, "\nSQL (truncated to %lu characters): ", maxLen);
            Ns_DStringNAppend(&ds, sql, (int)maxLen);
            Ns_DStringNAppend(&ds, "...", 3);
        } else {
            Ns_DStringPrintf(&ds, "\nSQL: %s", sql);
        }
    }
    Tcl_DStringResult(interp, &ds);

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
    enum { state_base, state_instr, state_bind } state;
    const char  *p;
    char        *bindbuf, *bp, *fragbuf, *fp, lastchar, nextchar;
    int          current_string_length = 0;
    size_t       inputLen;
    linkedListElement_t *elt,  *head=NULL,  *tail=NULL;
    linkedListElement_t *felt, *fhead=NULL, *ftail=NULL;
    Tcl_DString  ds;

    NS_NONNULL_ASSERT(input != NULL);
    NS_NONNULL_ASSERT(bind_variables != NULL);
    NS_NONNULL_ASSERT(fragments != NULL);

    Tcl_DStringInit(&ds);
    inputLen = strlen(input);
    fragbuf = ns_malloc((inputLen + 1U) * sizeof(char));
    fp = fragbuf;
    bindbuf = ns_malloc((inputLen + 1U) * sizeof(char));
    bp = bindbuf;

    for (p = input, state = state_base, lastchar = '\0'; *p != '\0'; lastchar = *p, p++) {

        switch (state) {
        case state_base:
            nextchar = *(p+1);
            if (unlikely(*p == '\'')) {
                state = state_instr;
                current_string_length = 0;
                *fp++ = *p;
            } else if ((*p == ':')
                       && (CHARTYPE(alpha, nextchar) != 0 || nextchar == '_')
                       && (lastchar != ':')) {
                bp = bindbuf;
                state = state_bind;
                *fp = '\0';
                felt = ListElementExternal("nobind", fragbuf, (int)(fp - fragbuf), &ds);

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

        case state_instr:
            if ((unlikely(*p == '\'')) && (lastchar != '\'' || current_string_length == 0)) {
                state = state_base;
            }
            current_string_length++;
            *fp++ = *p;
            break;

        case state_bind:
            if (unlikely(*p == '=')) {
                state = state_base;
                bp = bindbuf;
                fp = fragbuf;
            } else if (!(*p == '_' || *p == '$' || *p == '#' || CHARTYPE(alnum, *p) != 0)) {
                *bp = '\0';
                elt = ListElementExternal("bind", bindbuf, (int)(bp - bindbuf), &ds);

                if (tail == NULL) {
                    head = tail = elt;
                } else {
                    tail->next = elt;
                    tail = elt;
                }
                state = state_base;
                fp = fragbuf;
                p--;
            } else {
                *bp++ = *p;
            }
            break;
        }
    }

    if (state == state_bind) {
        *bp = '\0';
        elt = ListElementExternal("bind", bindbuf, (int)(bp - bindbuf), &ds);

        if (tail == NULL) {
            head = elt;
            /*tail = elt;*/
        } else {
            tail->next = elt;
            /*tail = elt;*/
        }
    } else {

        *fp = '\0';
        felt = ListElementExternal("nobind", fragbuf, (int)(fp - fragbuf), &ds);

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
    Tcl_DStringFree(&ds);

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
    const Connection *pconn;
    char             *segment_pos;
    int               segment = 1, result = TCL_OK;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(handle != NULL);
    NS_NONNULL_ASSERT(query != NULL);

    pconn = handle->connection;
    segment_pos = query + strlen(query);

    for (;;) {
        const unsigned char *data_column;
        const unsigned char *raw_data;
        int                  i, j;
        long                 byte_len, n;
        size_t               obtained_length;
        unsigned char        buf[6001];

        buf[0] = UCHAR('\0');
        sprintf(segment_pos, "%d", segment);
        if (Ns_DbExec(handle, query) != NS_ROWS) {
            Ns_TclPrintfResult(interp, "Error selecting data from BLOB");
            result = TCL_ERROR;
            break;
        }

        if (PQntuples(pconn->res) == 0) {
            break;
        }

        byte_len = strtol(PQgetvalue(pconn->res, 0, 0), NULL, 10);
        raw_data = (const unsigned char *)PQgetvalue(pconn->res, 0, 1);
        data_column = PQunescapeBytea(raw_data, &obtained_length);

        n = byte_len;
        for (i = 0, j = 0; n > 0; i += 4, j += 3, n -= 3) {
            decode3((const unsigned char*)&data_column[i], &buf[j], n);
        }

        PQfreemem((char*)data_column);

        if (fd != NS_INVALID_FD || conn != NULL) {
            (void) write_to_stream(fd, conn, buf, (size_t)byte_len, (conn != NULL) ? NS_TRUE : NS_FALSE);
        } else {
            Tcl_SetObjResult(interp, Tcl_NewByteArrayObj((const unsigned char *)buf, (int)byte_len));
        }
        segment++;
    }

    return result;
}


static int
blob_get(Tcl_Interp *interp, Ns_DbHandle *handle, const char *lob_id)
{
    Connection *pconn;
    char        query[100];
    int         result;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(handle != NULL);
    NS_NONNULL_ASSERT(lob_id != NULL);

    pconn = handle->connection;
    query[0] = '\0';
    strcpy(query, "SELECT BYTE_LEN, DATA FROM LOB_DATA WHERE LOB_ID = ");
    strcat(query, lob_id);
    strcat(query, " AND SEGMENT = ");

    result = get_blob_tuples(interp, handle, query, NULL, NS_INVALID_FD);

    PQclear(pconn->res);
    pconn->res = NULL;

    return result;
}

/* ns_pg blob_select_file db blob_id filename
 * Write a pseudo-blob to the passed in temp filename.  Some of this
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
                    bool to_conn_p, const char *filename)
{
    Connection  *pconn;
    Ns_Conn     *conn = NULL;
    int          fd = -1, result = TCL_OK;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(handle != NULL);
    NS_NONNULL_ASSERT(lob_id != NULL);

    pconn = handle->connection;
    if (to_conn_p) {
        conn = Ns_TclGetConn(interp);

        if (conn == NULL) {
            /*
             * This shouldn't happen, but spew an error just in case
             */
            Ns_Log(Error, "blob_send_to_stream: No connection available");
            Ns_TclPrintfResult(interp, "No connection available");
            result = TCL_ERROR;
        }

    } else {
        if (filename == NULL) {
            Ns_TclPrintfResult(interp, "could not create temporary file to spool "
                              "BLOB/CLOB result");
            result = TCL_ERROR;
        } else {
            fd = ns_open(filename, O_CREAT | O_TRUNC | O_WRONLY, 0600);

            if (fd < 0) {
                Ns_Log(Error, "Can't open %s for writing. error %d(%s)",
                       filename, errno, strerror(errno));
                Ns_TclPrintfResult(interp, "can't open file '%s' for writing. "
                                   "received error: %s",
                                   filename, strerror(errno));
                result = TCL_ERROR;
            }
        }
    }
    if (result == TCL_OK) {
        char         query[100];

        query[0] = '\0';
        strcpy(query, "SELECT BYTE_LEN, DATA FROM LOB_DATA WHERE LOB_ID = ");
        strcat(query, lob_id);
        strcat(query, " AND SEGMENT = ");

        result = get_blob_tuples(interp, handle, query, conn, fd);
    }

    if (!to_conn_p) {
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
write_to_stream(int fd, Ns_Conn *conn, const void *bufp, size_t length, bool to_conn_p)
{
    ssize_t bytes_written = 0;

    NS_NONNULL_ASSERT(bufp != NULL);
    assert(fd != NS_INVALID_FD || conn != NULL);

    if (to_conn_p) {
        size_t n = Ns_ConnContentSent(conn);

        if (Ns_ConnWriteData(conn, bufp, length, NS_CONN_STREAM) == NS_OK) {
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
blob_put(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, Tcl_Obj *valueObj)
{
    int                  segment, value_len, result = TCL_OK;
    unsigned char        out_buf[8001];
    const unsigned char *value_ptr;
    char                 query[10000], *segment_pos;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(handle != NULL);
    NS_NONNULL_ASSERT(blob_id != NULL);
    NS_NONNULL_ASSERT(valueObj != NULL);

    value_ptr = Tcl_GetByteArrayFromObj(valueObj, &value_len);

    query[0] = '\0';
    strcpy(query, "INSERT INTO LOB_DATA VALUES(");
    strcat(query, blob_id);
    strcat(query, ",");
    segment_pos = query + strlen(query);
    segment = 1;

    while (value_len > 0) {
        int i, j, segment_len = value_len > 6000 ? 6000 : value_len;

        value_len -= segment_len;
        for (i = 0, j = 0; i < segment_len; i += 3, j+=4) {
            encode3(&value_ptr[i], &out_buf[j]);
        }
        out_buf[j] = UCHAR('\0');

        sprintf(segment_pos, "%d, %d, '%s')", segment, segment_len, out_buf);
        if (Ns_DbExec(handle, query) != NS_DML) {
            Ns_TclPrintfResult(interp, "Error inserting data into BLOB");
            result = TCL_ERROR;
            break;
        } else {
            value_ptr += segment_len;
            segment++;
        }
    }

    return result;
}

/* ns_pg blob_dml_file blob_id file_name
 * Stuff the contents of file_name into the pseudo-blob blob_id
 */

static int
blob_dml_file(Tcl_Interp *interp, Ns_DbHandle *handle, const char *blob_id, const char *filename)
{
    int fd, result = TCL_OK;

    NS_NONNULL_ASSERT(interp != NULL);
    NS_NONNULL_ASSERT(handle != NULL);
    NS_NONNULL_ASSERT(blob_id != NULL);
    NS_NONNULL_ASSERT(filename != NULL);

    fd = ns_open(filename, O_RDONLY, 0);

    if (fd == NS_INVALID_FD) {
        Ns_Log(Error, " Error opening file %s: %d(%s)",
                filename, errno, strerror(errno));
        Ns_TclPrintfResult(interp, "can't open file '%s'for reading. "
                           "received error: %s ",
                           filename, strerror(errno));
        result = TCL_ERROR;
    } else {
        int           segment;
        ssize_t       readlen;
        char         *segment_pos;
        unsigned char in_buf[6000], out_buf[8001];
        char          query[10000];

        query[0] = '\0';
        strcpy(query, "INSERT INTO LOB_DATA VALUES(");
        strcat(query, blob_id);
        strcat(query, ",");
        segment_pos = query + strlen(query);
        segment = 1;

        readlen = ns_read(fd, in_buf, 6000u);
        while (readlen > 0) {
            int i, j;

            for (i = 0, j = 0; i < readlen; i += 3, j+=4) {
                encode3(&in_buf[i], &out_buf[j]);
            }
            out_buf[j] = UCHAR('\0');
            sprintf(segment_pos, "%d, %" PRIdz ", '%s')", segment, readlen, out_buf);
            if (Ns_DbExec(handle, query) != NS_DML) {
                Ns_TclPrintfResult(interp, "Error inserting data into BLOB");
                result = TCL_ERROR;
                break;
            } else {
                readlen = ns_read(fd, in_buf, 6000u);
                segment++;
            }
        }
        (void) ns_close(fd);
    }

    return result;
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
linkedListElement_new(const char *chars, int length)
{
    linkedListElement_t *elt;

    NS_NONNULL_ASSERT(chars != NULL);

    elt = ns_malloc(sizeof(linkedListElement_t));
    elt->chars = chars;
    elt->length = length;
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
    int n = 0;

    for (; head != NULL; head = head->next) {
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
LinkedList_free_list(linkedListElement_t *head)
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
 * strings.  The uuencoding algorithm doesn't make use of lowercase letters,
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
    c = UCHAR((c & 0x3Fu) + UCHAR(' '));
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
    unsigned char u1, u2, u3, u4;

    *buf++ = enc_one(*p >> 2);
    /*
     * At least gcc 9 complains, when the expression behind u1 and u2 are used
     * directly as arguments.
     */
    u1 = (*p << 4)   & UCHAR(0x30u);
    u2 = (p[1] >> 4) & UCHAR(0x0Fu);
    *buf++ = enc_one(u1 | u2);
    /*
     * Use fresh variables, since this allow better pipelining/vectorization.
     */
    u3 = (p[1] << 2) & UCHAR(0x3Cu);
    u4 = (p[2] >> 6) & UCHAR(0x03u);
    *buf++ = enc_one(u3 | u4);
    *buf++ = enc_one(p[2] & UCHAR(0x3Fu));
}


/* single-character decode */
#define DEC(c)  ((UCHAR(c) - UCHAR(' ')) & UCHAR(0x3Fu))

static unsigned char
get_one(unsigned char c)
{
    if (c == UCHAR('a')) {
        c = UCHAR('\'');
    } else if (c == UCHAR('b')) {
        c = UCHAR('\\');
    }
    return c;
}

static void
decode3(const unsigned char *p, unsigned char *buf, long n)
{
    unsigned char c1, c2, c3, c4;

    NS_NONNULL_ASSERT(p != NULL);
    NS_NONNULL_ASSERT(buf != NULL);

    c1 = get_one(p[0]);
    c2 = get_one(p[1]);
    c3 = get_one(p[2]);
    c4 = get_one(p[3]);

    if (n >= 1) {
        *buf++ = UCHAR(UCHAR(DEC(c1) << 2) | DEC(c2) >> 4);
    }
    if (n >= 2) {
        *buf++ = UCHAR(UCHAR(DEC(c2) << 4) | DEC(c3) >> 2);
    }

    if (n >= 3) {
        *buf++ = UCHAR(UCHAR(DEC(c3) << 6) | DEC(c4));
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
