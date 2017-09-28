/*
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License");  you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is Protege-2000.
 *
 * The Initial Developer of the Original Code is Stanford University. Portions
 * created by Stanford University are Copyright (C) 2001.  All Rights Reserved.
 *
 * Protege-2000 was developed by Stanford Medical Informatics
 * (http://www.smi.stanford.edu) at the Stanford University School of Medicine
 * with support from the National Library of Medicine, the National Science
 * Foundation, and the Defense Advanced Research Projects Agency.  Current
 * information about Protege can be obtained at http://protege.stanford.edu
 *
 * Contributor(s):
 */

package edu.stanford.smi.protege.storage.jdbc;

import java.util.*;
import java.sql.*;
import edu.stanford.smi.protege.util.*;

/**
 * Cache for prepared statements
 *
 * @author Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class PreparedStatementHolder {
    private Connection _connection;
    private Map _statements = new HashMap();
    private int counter;

    public PreparedStatementHolder(Connection c) {
        _connection = c;
    }

    public void closeAll() throws SQLException {
        // Log.trace("Closing " + _statements.size() + " statements", this, "closeAll");
        Iterator i = _statements.values().iterator();
        while (i.hasNext()) {
            PreparedStatement stmt = (PreparedStatement) i.next();
            stmt.close();
        }
        _statements.clear();
    }

    public PreparedStatement get(String stmtText) throws SQLException {
        PreparedStatement result = (PreparedStatement) _statements.get(stmtText);
        if (result == null) {
            result = _connection.prepareStatement(stmtText);
            _statements.put(stmtText, result);
        } else {
            /* This would be a nice check to ensure that everything is set correctly.
             * Unfortunately the stupid jdbc:odbc bridge throws a NullPointerException
             * when this method is called if the statement has no parameters (JDK 1.3)
             */
            // result.clearParameters();
        }
        return result;
    }

    private Integer getCounter() {
        return new Integer(++counter);
    }
}
