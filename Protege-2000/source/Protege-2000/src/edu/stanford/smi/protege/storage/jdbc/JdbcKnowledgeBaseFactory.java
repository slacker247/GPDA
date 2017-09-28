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

import java.sql.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.model.framedb.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class JdbcKnowledgeBaseFactory implements KnowledgeBaseFactory {
    final static String USERNAME = "username";
    final static String PASSWORD = "password";
    final static String URL = "url";
    final static String DRIVER = "driver";
    final static String TABLENAME = "table";

    public JdbcKnowledgeBaseFactory() {
        // Log.enter(this, "JdbcKnowledgeBaseFactory");
    }

    private void copyKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        String driver = getDriver(sources);
        String url = getURL(sources);
        String username = getUsername(sources);
        String password = getPassword(sources);
        String tablename = getTableName(sources);
        copyKnowledgeBase(kb, driver, url, tablename, username, password, errors);
    }

    private void copyKnowledgeBase(KnowledgeBase kb, String driver, String url, String tablename,
            String username, String password, Collection errors) {
        try {
            DatabaseManager manager = new SimpleJdbcDatabaseManager(driver, url, tablename,
                    username, password, errors);
            manager.saveKnowledgeBase(kb);
            manager.dispose();
        } catch (Exception e) {
            errors.add(e);
        }
    }

    public KnowledgeBase createKnowledgeBase(Collection errors) {
        DefaultKnowledgeBase kb = new DefaultKnowledgeBase(this);
        return kb;
    }

    public KnowledgeBaseSourcesEditor createKnowledgeBaseSourcesEditor(String projectName, PropertyList sources) {
        return new JdbcKnowledgeBaseSourcesEditor(projectName, sources);
    }

    public String getDescription() {
        return "JDBC Database";
    }

    public static String getDriver(PropertyList sources) {
        return sources.getString(DRIVER);
    }

    public static String getPassword(PropertyList sources) {
        return sources.getString(PASSWORD);
    }

    public String getProjectFilePath() {
        return null;
    }

    public static String getTableName(PropertyList sources) {
        return sources.getString(TABLENAME);
    }

    public static String getURL(PropertyList sources) {
        return sources.getString(URL);
    }

    public static String getUsername(PropertyList sources) {
        return sources.getString(USERNAME);
    }

    public void includeKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        // TODO
    }

    public boolean isComplete(PropertyList sources) {
        String tableName = getTableName(sources);
        String url = getURL(sources);
        String driver = getDriver(sources);
        return tableName != null && url != null && driver != null;
    }

    public void loadKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        String driver = getDriver(sources);
        String url = getURL(sources);
        if (driver != null && url != null) {
            String username = getUsername(sources);
            String password = getPassword(sources);
            String tablename = getTableName(sources);
            loadKnowledgeBase(kb, driver, tablename, url, username, password, errors);
        }
    }

    public void loadKnowledgeBase(
        KnowledgeBase kb,
        String driver,
        String tableName,
        String url,
        String username,
        String password,
        Collection errors) {
        try {
            DefaultKnowledgeBase dkb = (DefaultKnowledgeBase) kb;
            SimpleJdbcDatabaseManager manager = new SimpleJdbcDatabaseManager(driver, url, tableName, username, password, errors);
            FrameDBStorage memoryStorage = (FrameDBStorage) dkb.getStorage();
            memoryStorage.setCaching(true);
            dkb.setStorage(new DatabaseStorage(dkb, manager, memoryStorage));
            dkb.setFrameIDAllocator(manager.getFrameIDAllocator());
        } catch (Exception e) {
            errors.add(e);
        }
    }

    public void saveKnowledgeBase(KnowledgeBase kb, PropertyList sources, Collection errors) {
        Storage storage = ((DefaultKnowledgeBase) kb).getStorage();
        if (storage instanceof DatabaseStorage) {
            DatabaseManager databaseManager = ((DatabaseStorage) storage).getDatabaseManager();
            if (!databaseManager.getTableName().equals(getTableName(sources))) {
                copyKnowledgeBase(kb, sources, errors);
            }
        } else {
            copyKnowledgeBase(kb, sources, errors);
        }
    }

    public static void setDriver(PropertyList sources, String driver) {
        sources.setString(DRIVER, driver);
    }

    public static void setPassword(PropertyList sources, String password) {
        sources.setString(PASSWORD, password);
    }

    public static void setTablename(PropertyList sources, String tablename) {
        sources.setString(TABLENAME, tablename);
    }

    public static void setURL(PropertyList sources, String url) {
        sources.setString(URL, url);
    }

    public static void setUsername(PropertyList sources, String username) {
        sources.setString(USERNAME, username);
    }

    public String toString() {
        return "JdbcKnowledgeBaseFactory";
    }
}
