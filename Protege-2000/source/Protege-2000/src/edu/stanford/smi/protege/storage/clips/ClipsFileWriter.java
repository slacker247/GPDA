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

package edu.stanford.smi.protege.storage.clips;


import java.io.*;
import java.util.*;
import edu.stanford.smi.protege.model.*;
import edu.stanford.smi.protege.resource.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ClipsFileWriter {
    private PrintWriter _writer;

    public ClipsFileWriter(Writer writer) {
        _writer = new PrintWriter(writer);
        printVersion();
    }

    public void flush() {
        _writer.flush();
    }

    public void print(int i) {
        _writer.print(i);
    }

    public void print(String s) {
        _writer.print(s);
    }

    public void printFrame(Frame frame) {
        printSymbol(frame.getName());
    }

    public void println() {
        _writer.println();
    }

    public void println(String s) {
        _writer.println(s);
    }

    public boolean printSucceeded() {
        return !_writer.checkError();
    }

    public void printSymbol(String symbol) {
        print(ClipsUtil.toExternalSymbol(symbol));
    }

    private void printVersion() {
        println("; " + new Date().toString());
        println("; ");
        println(";+ (version \"" + Text.getVersion() + "\")");
        println(";+ (build \"" + Text.getBuildInfo() + "\")");
    }

    public String toExternalFrameName(Frame frame) {
        return ClipsUtil.toExternalSymbol(frame.getName());
    }
}
