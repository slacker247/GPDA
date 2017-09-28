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

package edu.stanford.smi.protege.util;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import edu.stanford.smi.protege.resource.*;

/**
 *  Description of the class
 *
 * @author    Ray Fergerson <fergerson@smi.stanford.edu>
 */
public class ModalDialog extends JDialog implements Disposable {
    public final static int OPTION_OK = 1;
    public final static int OPTION_YES = 2;
    public final static int OPTION_NO = 3;
    public final static int OPTION_CANCEL = 4;
    public final static int OPTION_CLOSE = 5;
    public final static int ERROR = 6;

    public final static int MODE_OK_CANCEL = 11;
    public final static int MODE_YES_NO_CANCEL = 12;
    public final static int MODE_YES_NO = 13;
    public final static int MODE_CLOSE = 14;
    // public static final int MODE_NONE            = 15;
    private int _result;
    private Component _panel;
    private JPanel _buttonsPanel;
    private CloseCallback _closeCallback;
    private boolean _enableCloseButton;

    private static ModalDialog _currentDialog;  // used for testing

    public static interface CloseCallback {
        boolean canClose(int result);
    }

    private class WindowCloseListener extends WindowAdapter {
        public void windowClosing(WindowEvent event) {
            int option = OPTION_CANCEL;
            if (!_enableCloseButton) {
                int result = ModalDialog.showMessageDialog(ModalDialog.this, "Do you want to save any changes?", ModalDialog.MODE_YES_NO);
                if (result == OPTION_YES) {
                    option = OPTION_OK;
                }
            }
            attemptClose(option);
        }
    }

    private ModalDialog(Dialog parentDialog, Component panel, String title, int mode,
            CloseCallback callback, boolean enableCloseButton) {
        super(parentDialog, title, true);
        init(panel, mode, callback, enableCloseButton);
    }

    private ModalDialog(Frame parentFrame, Component panel, String title, int mode,
            CloseCallback callback, boolean enableCloseButton) {
        super(parentFrame, title, true);
        init(panel, mode, callback, enableCloseButton);
    }

    public void attemptClose(int result) {
        boolean canClose;
        if (_closeCallback == null) {
            canClose = true;
        } else {
            canClose = _closeCallback.canClose(result);
        }
        if (canClose && result == OPTION_OK && _panel instanceof Validatable) {
            Validatable validatable = (Validatable) _panel;
            canClose = validatable.validateContents();
            if (canClose) {
                validatable.saveContents();
            }
        }
        if (canClose) {
            _result = result;
            close();
        }
    }

    private void close() {
        Component c = SwingUtilities.getRoot(getOwner());
        ComponentUtilities.dispose(this);
        // Hack for JDK 1.3 repaint problems
        c.repaint();
        _currentDialog = null;
    }

    private JButton createButton(final int result, String text, Icon icon) {
        Action action =
            new AbstractAction(text, icon) {
                public void actionPerformed(ActionEvent event) {
                    attemptClose(result);
                }
            }
        ;
        JButton button = ComponentFactory.createButton(action);
        button.addKeyListener(
            new KeyAdapter() {
                public void keyPressed(KeyEvent event) {
                    if (event.getKeyCode() == KeyEvent.VK_ENTER) {
                        attemptClose(result);
                    }
                }
            }
        );
        return button;
    }

    private JPanel createButtonsPanel(int mode) {
        JPanel buttonsGrid = ComponentFactory.createPanel();
        buttonsGrid.setLayout(new GridLayout(1, 3, 10, 10));
        switch (mode) {
            case MODE_OK_CANCEL:
                buttonsGrid.add(createButton(OPTION_OK, "OK", Icons.getOkIcon()));
                buttonsGrid.add(createButton(OPTION_CANCEL, "Cancel", Icons.getCancelIcon()));
                break;
            case MODE_YES_NO:
                buttonsGrid.add(createButton(OPTION_YES, "Yes", Icons.getYesIcon()));
                buttonsGrid.add(createButton(OPTION_NO, "No", Icons.getNoIcon()));
                break;
            case MODE_YES_NO_CANCEL:
                buttonsGrid.add(createButton(OPTION_YES, "Yes", Icons.getYesIcon()));
                buttonsGrid.add(createButton(OPTION_NO, "No", Icons.getNoIcon()));
                buttonsGrid.add(createButton(OPTION_CANCEL, "Cancel", Icons.getCancelIcon()));
                break;
            case MODE_CLOSE:
                buttonsGrid.add(createButton(OPTION_CLOSE, "Close", Icons.getCloseIcon()));
                break;
            default:
        }

        JPanel panel = ComponentFactory.createPanel();
        panel.setLayout(new FlowLayout());
        panel.add(buttonsGrid);

        return panel;
    }

    public void finalize() {
        try {
            super.finalize();
            // System.out.println(getClass().getName() + " finalize");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    public static ModalDialog getCurrentDialog() {
        return _currentDialog;
    }

    //  Doesn't work!
    private void getFocus() {
        SwingUtilities.invokeLater(
            new Runnable() {
                public void run() {
                    JButton button = (JButton) ((Container) _buttonsPanel.getComponent(0)).getComponent(0);
                    button.requestFocus();
                    // Log.trace("requesting focus: " + button, this, "run");
                }
            }
        );
    }

    private void init(Component panel, int mode, CloseCallback callback, boolean enableCloseButton) {
        _currentDialog = this;
        _closeCallback = callback;
        _enableCloseButton = enableCloseButton;
        setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        addWindowListener(new WindowCloseListener());
        setBackground(Color.lightGray);

        // Only allow the close button to work on simple confirmation dialogs
        // if (panel instanceof MessagePanel) {
        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent event) {
                attemptClose(OPTION_CANCEL);
            }
        });
        // }

        switch (mode) {
            case MODE_OK_CANCEL :
                _result = OPTION_CANCEL;
                break;
            case MODE_YES_NO_CANCEL :
                _result = OPTION_CANCEL;
                break;
            case MODE_CLOSE :
                _result = OPTION_CLOSE;
                break;
        }

        _panel = panel;
        _buttonsPanel = createButtonsPanel(mode);

        layoutWidgets();

        Rectangle r = null;
        // (Rectangle) theirBounds.get(panel.getClass());
        if (r == null) {
            pack();
            ComponentUtilities.center(this);
        } else {
            setBounds(r);
        }
        getFocus();
        show();
    }

    private void layoutWidgets() {
        JPanel borderedPanel = ComponentFactory.createPanel();
        borderedPanel.setLayout(new BorderLayout());
        borderedPanel.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
        borderedPanel.add(_panel);

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(borderedPanel, BorderLayout.CENTER);
        getContentPane().add(_buttonsPanel, BorderLayout.SOUTH);
    }

    // private static Map theirBounds = new HashMap();
    public static int showDialog(Component parent, Component panel, String title, int mode) {
        return showDialog(parent, panel, title, mode, null);
    }

    public static int showDialog(Component parent, Component panel, String title, int mode,
            CloseCallback callback) {
        return showDialog(parent, panel, title, mode, callback, true);
    }

    public static int showDialog(Component parent, Component panel, String title, int mode,
            CloseCallback callback, boolean enableCloseButton) {
        ModalDialog dialog;
        Window window;
        if (parent instanceof Window) {
            window = (Window) parent;
        } else {
            window = SwingUtilities.windowForComponent(parent);
        }
        if (window instanceof Frame) {
            dialog = new ModalDialog((Frame) window, panel, title, mode, callback, enableCloseButton);
        } else if (window instanceof Dialog) {
            dialog = new ModalDialog((Dialog) window, panel, title, mode, callback, enableCloseButton);
        } else {
            Log.error("Unable to create dialog for window: " + window, ModalDialog.class, "showDialog", parent, panel, title);
            dialog = null;
        }
        int result;
        if (dialog == null) {
            result = ERROR;
        } else {
            result = dialog._result;
            // theirBounds.put(panel.getClass(), dialog.getBounds());
        }
        return result;
    }

    public static void showMessageDialog(Component parent, String message) {
        showMessageDialog(parent, message, ModalDialog.MODE_CLOSE);
    }

    public static int showMessageDialog(Component parent, String message, int mode) {
        return showDialog(parent, new MessagePanel(message), "", mode);
    }
}
