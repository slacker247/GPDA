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



import java.awt.*;
import java.io.*;
import javax.swing.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.util.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class JdbcKnowledgeBaseSourcesEditor extends KnowledgeBaseSourcesEditor {
    private final static String DEFAULT_DRIVER = "sun.jdbc.odbc.JdbcOdbcDriver";
    private final static String DEFAULT_URL = "jdbc:odbc:ProtegeDB";
    private final static String DEFAULT_PASSWORD = "";
    private final static String DEFAULT_TABLE_NAME = "ProtegeTable";

    private JTextField _driverComponent;
    private JTextField _urlComponent;
    private JTextField _tableNameComponent;
    private JTextField _usernameComponent;
    private JTextField _passwordComponent;

    public JdbcKnowledgeBaseSourcesEditor(String projectName, PropertyList sources) {
        super(projectName, sources);
        JPanel panel = new JPanel();
        panel.setLayout(new GridLayout(5, 1));
        panel.add(createDriverComponent());
        panel.add(createURLComponent());
        panel.add(createTableNameComponent());
        panel.add(createUsernameComponent());
        panel.add(createPasswordComponent());
        add(panel);
    }

    public Component createDriverComponent() {
        _driverComponent = ComponentFactory.createTextField();
        String text = JdbcKnowledgeBaseFactory.getDriver(getSources());
        if (text == null) {
            text = getProperty(JdbcKnowledgeBaseFactory.DRIVER, DEFAULT_DRIVER);
        }
        _driverComponent.setText(text);
        return new LabeledComponent("JDBC Driver Class Name", _driverComponent);
    }

    private Component createPasswordComponent() {
        _passwordComponent = ComponentFactory.createPasswordField();
        String text = JdbcKnowledgeBaseFactory.getPassword(getSources());
        _passwordComponent.setText(text);
        if (text == null) {
            text = DEFAULT_PASSWORD;
        }
        return new LabeledComponent("Password", _passwordComponent);
    }

    private Component createTableNameComponent() {
        _tableNameComponent = ComponentFactory.createTextField();
        String text = JdbcKnowledgeBaseFactory.getTableName(getSources());
        if (text == null) {
            text = getDefaultTableName();
        }
        _tableNameComponent.setText(text);
        return new LabeledComponent("Table", _tableNameComponent);
    }

    private Component createURLComponent() {
        _urlComponent = ComponentFactory.createTextField();
        String text = JdbcKnowledgeBaseFactory.getURL(getSources());
        if (text == null) {
            text = getProperty(JdbcKnowledgeBaseFactory.URL, DEFAULT_URL);
        }
        _urlComponent.setText(text);
        return new LabeledComponent("JDBC URL", _urlComponent);
    }

    private Component createUsernameComponent() {
        _usernameComponent = ComponentFactory.createTextField();
        String text = JdbcKnowledgeBaseFactory.getUsername(getSources());
        if (text == null) {
            text = getProperty(JdbcKnowledgeBaseFactory.USERNAME, null);
        }
        if (text == null) {
            text = SystemUtilities.getSystemProperty("user.name");
        }
        _usernameComponent.setText(text);
        return new LabeledComponent("Username", _usernameComponent);
    }

    private String getDefaultTableName() {
        String tableName = getProperty(JdbcKnowledgeBaseFactory.TABLENAME, null);
        if (tableName == null) {
            String path = getProjectPath();
            if (path != null) {
                tableName = getName(path);
            }
        }
        if (tableName == null) {
            tableName = DEFAULT_TABLE_NAME;
        }
        return tableName;
    }

    private static String getName(String path) {
        String name;
        if (path == null) {
            name = "";
        } else {
            name = new File(path).getName();
            int index = name.indexOf('.');
            if (index >= 0) {
                name = name.substring(0, index);
            }
        }
        return name;
    }

    private String getProperty(String name, String defaultValue) {
        return ApplicationProperties.getString(getPropertyName(name), defaultValue);
    }

    private String getPropertyName(String baseName) {
        return getClass().getName() + "." + baseName;
    }

    private String getTableName() {
        return _tableNameComponent.getText();
    }

    protected void onProjectPathChange(String oldPath, String newPath) {
        // do nothing
    }

    public void saveContents() {
        String driver = _driverComponent.getText();
        JdbcKnowledgeBaseFactory.setDriver(getSources(), driver);
        setProperty(JdbcKnowledgeBaseFactory.DRIVER, driver);

        String url = _urlComponent.getText();
        JdbcKnowledgeBaseFactory.setURL(getSources(), url);
        setProperty(JdbcKnowledgeBaseFactory.URL, url);

        String tableName = _tableNameComponent.getText();
        JdbcKnowledgeBaseFactory.setTablename(getSources(), tableName);
        setProperty(JdbcKnowledgeBaseFactory.TABLENAME, tableName);

        String userName = _usernameComponent.getText();
        JdbcKnowledgeBaseFactory.setUsername(getSources(), userName);
        setProperty(JdbcKnowledgeBaseFactory.USERNAME, userName);

        JdbcKnowledgeBaseFactory.setPassword(getSources(), _passwordComponent.getText());
    }

    private void setProperty(String name, String value) {
        ApplicationProperties.setString(getPropertyName(name), value);
    }

    public boolean validateContents() {
        return true;
    }
}
