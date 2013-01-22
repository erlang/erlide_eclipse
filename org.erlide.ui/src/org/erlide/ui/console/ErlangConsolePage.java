/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.console;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.ResourceBundle;

import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.debug.internal.ui.DebugUIPlugin;
import org.eclipse.debug.internal.ui.preferences.IDebugPreferenceConstants;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IFindReplaceTarget;
import org.eclipse.jface.text.ITextOperationTarget;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.FocusListener;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.actions.TextViewerAction;
import org.eclipse.ui.internal.console.ConsoleMessages;
import org.eclipse.ui.internal.console.ConsoleResourceBundleMessages;
import org.eclipse.ui.internal.console.IConsoleHelpContextIds;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.texteditor.FindReplaceAction;
import org.eclipse.ui.texteditor.IUpdate;
import org.erlide.backend.BackendException;
import org.erlide.backend.BackendHelper;
import org.erlide.backend.console.IBackendShell;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

@SuppressWarnings("restriction")
public class ErlangConsolePage extends Page implements IAdaptable,
        IPropertyChangeListener {
    public static final String ID = "org.erlide.ui.views.console";

    Color bgColor_Ok;
    Color bgColor_Err;

    StyledText consoleText;
    private final ErlConsoleDocument fDoc;
    final ErlangConsoleHistory history = new ErlangConsoleHistory();
    StyledText consoleInput;
    SourceViewer consoleOutputViewer;
    private SourceViewer consoleInputViewer;
    private IBackendShell shell;
    protected Map<String, IAction> fGlobalActions = new HashMap<String, IAction>();
    protected ArrayList<String> fSelectionActions = new ArrayList<String>();
    // protected ClearOutputAction fClearOutputAction;
    private final ErlangConsole fConsole;
    private IConsoleView fConsoleView;
    private MenuManager fMenuManager;

    private final ISelectionChangedListener selectionChangedListener = new ISelectionChangedListener() {
        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            updateSelectionDependentActions();
        }
    };

    private Composite composite;

    private boolean disposeColors;

    public ErlangConsolePage(final IConsoleView view,
            final ErlangConsole console) {
        super();
        fConsole = console;
        fConsoleView = view;
        shell = console.getShell();
        fDoc = new ErlConsoleDocument(shell);
    }

    @Override
    public void dispose() {
        consoleOutputViewer.getSelectionProvider()
                .removeSelectionChangedListener(selectionChangedListener);
        if (fMenuManager != null) {
            fMenuManager.dispose();
        }
        // fClearOutputAction = null;
        fSelectionActions.clear();
        fGlobalActions.clear();

        shell.dispose();
        shell = null;

        fConsoleView = null;

        if (disposeColors) {
            bgColor_Err.dispose();
            bgColor_Ok.dispose();
        }
        super.dispose();
    }

    boolean isInputComplete() {
        try {
            final String str = consoleInput.getText() + " ";
            final BackendHelper helper = new BackendHelper();
            final OtpErlangObject o = helper.parseConsoleInput(str);
            if (o instanceof OtpErlangList && ((OtpErlangList) o).arity() == 0) {
                return false;
            }
            if (!(o instanceof OtpErlangList)) {
                return false;
            }
        } catch (final BackendException e) {
            return false;
        }
        return true;
    }

    protected void sendInput() {
        final String s = consoleInput.getText();
        input(s);
        consoleInput.setText("");
    }

    public void input(final String data) {
        final String data2 = data.trim() + "\n";
        shell.input(data2);
        shell.send(data2);
        history.addToHistory(data.trim());
    }

    public void setInput(final String str) {
        consoleInput.setText(str);
        consoleInput.setSelection(str.length());
    }

    /**
     * @wbp.parser.entryPoint
     */
    @Override
    public void createControl(final Composite parent) {
        setBackgroundColors();

        composite = new Composite(parent, SWT.NONE);
        composite.setLayout(new GridLayout(1, false));

        final SashForm sashForm = new SashForm(composite, SWT.VERTICAL);
        sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        consoleOutputViewer = new SourceViewer(sashForm, null, SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.MULTI | SWT.READ_ONLY);
        consoleText = consoleOutputViewer.getTextWidget();
        consoleText.setFont(JFaceResources.getTextFont());
        final Color bgcolor = DebugUIPlugin
                .getPreferenceColor(IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR);
        consoleText.setBackground(bgcolor);
        consoleText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                if (fConsole.isStopped()) {
                    return;
                }
                final boolean isHistoryCommand = (e.stateMask & SWT.CTRL) == SWT.CTRL
                        && (e.keyCode == SWT.ARROW_UP || e.keyCode == SWT.ARROW_DOWN);
                if (e.character != (char) 0 || isHistoryCommand) {
                    e.doit = false;
                }
            }

        });
        consoleText.addFocusListener(new FocusListener() {
            @Override
            public void focusLost(final FocusEvent e) {
            }

            @Override
            public void focusGained(final FocusEvent e) {
                consoleInput.setFocus();
            }
        });
        DebugUIPlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new IPropertyChangeListener() {
                    @Override
                    public void propertyChange(final PropertyChangeEvent event) {
                        if (event
                                .getProperty()
                                .equals(IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR)) {
                            final Color color = DebugUIPlugin
                                    .getPreferenceColor(IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR);
                            consoleText.setBackground(color);
                            consoleInput.setBackground(color);
                        }
                    }
                });

        consoleOutputViewer.setDocument(fDoc);
        consoleOutputViewer
                .configure(new ErlangConsoleSourceViewerConfiguration());

        consoleInputViewer = new SourceViewer(sashForm, null, SWT.MULTI
                | SWT.WRAP | SWT.V_SCROLL);
        consoleInputViewer.setDocument(new Document());
        consoleInputViewer
                .configure(new ErlangConsoleSourceViewerConfiguration());
        consoleInput = (StyledText) consoleInputViewer.getControl();
        consoleInput.setBackground(bgcolor);

        sashForm.setWeights(new int[] { 2, 1 });

        final Label helpLabel = new Label(composite, SWT.NONE);
        helpLabel
                .setText("Press Ctrl-Enter to send the input to the console. Press Esc to clear input.");
        helpLabel.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        consoleInput.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                final boolean ctrlPressed = (e.stateMask & SWT.CTRL) == SWT.CTRL;
                if (e.keyCode == 13 && ctrlPressed && isInputComplete()) {
                    sendInput();
                    e.doit = true;
                } else if (ctrlPressed && e.keyCode == SWT.ARROW_UP) {
                    history.prev();
                    final String s = history.get();
                    if (s != null) {
                        consoleInput.setText(s);
                        consoleInput.setSelection(consoleInput.getText()
                                .length());
                    }
                } else if (ctrlPressed && e.keyCode == SWT.ARROW_DOWN) {
                    history.next();
                    final String s = history.get();
                    if (s != null) {
                        consoleInput.setText(s);
                        consoleInput.setSelection(consoleInput.getText()
                                .length());
                    }
                } else if (e.keyCode == SWT.ESC) {
                    consoleInput.setText("");
                }
            }
        });
        consoleInput.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                if (isInputComplete()) {
                    consoleInput.setBackground(bgColor_Ok);
                } else {
                    final Color bgColorErr = bgColor_Err;
                    consoleInput.setBackground(bgColorErr);
                }
            }
        });
        consoleInput.setFont(consoleText.getFont());
        consoleInput.setBackground(consoleText.getBackground());
        consoleInput.setWordWrap(true);
        consoleInput.setFocus();

        // end layout

        final IDocumentListener documentListener = new IDocumentListener() {
            @Override
            public void documentAboutToBeChanged(final DocumentEvent event) {
            }

            @Override
            public void documentChanged(final DocumentEvent event) {
                final int end = consoleOutputViewer.getDocument().getLength();
                consoleOutputViewer.setSelectedRange(end, end);
                consoleOutputViewer.revealRange(end, 0);
            }
        };
        fDoc.addDocumentListener(documentListener);

        final String id = "#ContextMenu"; //$NON-NLS-1$
        // if (getConsole().getType() != null) {
        //			id = getConsole().getType() + "." + id; //$NON-NLS-1$
        // }
        fMenuManager = new MenuManager("#ContextMenu", id); //$NON-NLS-1$
        fMenuManager.setRemoveAllWhenShown(true);
        fMenuManager.addMenuListener(new IMenuListener() {
            @Override
            public void menuAboutToShow(final IMenuManager m) {
                contextMenuAboutToShow(m);
            }
        });
        final Menu menu = fMenuManager.createContextMenu(getControl());
        getControl().setMenu(menu);

        createActions();
        configureToolBar(getSite().getActionBars().getToolBarManager());

        getSite().registerContextMenu(id, fMenuManager, consoleOutputViewer);
        getSite().setSelectionProvider(consoleOutputViewer);
        consoleOutputViewer.getSelectionProvider().addSelectionChangedListener(
                selectionChangedListener);

    }

    private void setBackgroundColors() {
        final Color color = DebugUIPlugin
                .getPreferenceColor(IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR);

        final float[] hsbvals = new float[3];
        java.awt.Color.RGBtoHSB(color.getRed(), color.getGreen(),
                color.getBlue(), hsbvals);

        if (hsbvals[1] >= 0.01) {
            bgColor_Ok = color;
            bgColor_Err = color;
            disposeColors = false;
        } else {
            final float red = java.awt.Color.RGBtoHSB(255, 0, 0, null)[0];
            final float green = java.awt.Color.RGBtoHSB(0, 255, 0, null)[0];
            final float deltaSaturation = 0.05f;

            int rgb = java.awt.Color.HSBtoRGB(red, deltaSaturation, hsbvals[2]);
            java.awt.Color cx = new java.awt.Color(rgb);
            bgColor_Err = new Color(Display.getCurrent(), new RGB(cx.getRed(),
                    cx.getGreen(), cx.getBlue()));

            rgb = java.awt.Color.HSBtoRGB(green, deltaSaturation, hsbvals[2]);
            cx = new java.awt.Color(rgb);
            bgColor_Ok = new Color(Display.getCurrent(), new RGB(cx.getRed(),
                    cx.getGreen(), cx.getBlue()));
            disposeColors = true;
        }
    }

    @Override
    public Control getControl() {
        return composite;
    }

    @Override
    public void setActionBars(final IActionBars bars) {
    }

    @Override
    public void setFocus() {
        consoleInput.setFocus();
    }

    @Override
    public Object getAdapter(@SuppressWarnings("rawtypes") final Class required) {
        if (IFindReplaceTarget.class.equals(required)) {
            return consoleOutputViewer.getFindReplaceTarget();
        }
        if (Widget.class.equals(required)) {
            return consoleOutputViewer.getTextWidget();
        }
        return null;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent event) {
        if (consoleOutputViewer != null) {
            final Object source = event.getSource();
            final String property = event.getProperty();

            if (source.equals(fConsole)
                    && IConsoleConstants.P_FONT.equals(property)) {
                // consoleOutputViewer.setFont(fConsole.getFont());
            } else if (IConsoleConstants.P_FONT_STYLE.equals(property)) {
                consoleOutputViewer.getTextWidget().redraw();
            } else if (property.equals(IConsoleConstants.P_STREAM_COLOR)) {
                consoleOutputViewer.getTextWidget().redraw();
            } else if (source.equals(fConsole)
                    && property.equals(IConsoleConstants.P_TAB_SIZE)) {
                // Integer tabSize = (Integer) event.getNewValue();
                // consoleOutputViewer.setTabWidth(tabSize.intValue());
            } else if (source.equals(fConsole)
                    && property.equals(IConsoleConstants.P_CONSOLE_WIDTH)) {
                // consoleOutputViewer.setConsoleWidth(fConsole.getConsoleWidth());
            } else if (IConsoleConstants.P_BACKGROUND_COLOR.equals(property)) {
                consoleOutputViewer.getTextWidget().setBackground(
                        fConsole.getBackground());
            }
        }
    }

    protected void createActions() {
        final IActionBars actionBars = getSite().getActionBars();
        TextViewerAction action = new TextViewerAction(consoleOutputViewer,
                ITextOperationTarget.SELECT_ALL);
        action.configureAction(ConsoleMessages.TextConsolePage_SelectAllText,
                ConsoleMessages.TextConsolePage_SelectAllDescrip,
                ConsoleMessages.TextConsolePage_SelectAllDescrip);
        action.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_SELECT_ALL);
        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(action,
                        IConsoleHelpContextIds.CONSOLE_SELECT_ALL_ACTION);
        setGlobalAction(actionBars, ActionFactory.SELECT_ALL.getId(), action);

        action = new TextViewerAction(consoleOutputViewer,
                ITextOperationTarget.CUT);
        action.configureAction(ConsoleMessages.TextConsolePage_CutText,
                ConsoleMessages.TextConsolePage_CutDescrip,
                ConsoleMessages.TextConsolePage_CutDescrip);
        action.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_CUT));
        action.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_CUT);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(action, IConsoleHelpContextIds.CONSOLE_CUT_ACTION);
        setGlobalAction(actionBars, ActionFactory.CUT.getId(), action);

        action = new TextViewerAction(consoleOutputViewer,
                ITextOperationTarget.COPY);
        action.configureAction(ConsoleMessages.TextConsolePage_CopyText,
                ConsoleMessages.TextConsolePage_CopyDescrip,
                ConsoleMessages.TextConsolePage_CopyDescrip);
        action.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
                .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
        action.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_COPY);
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(action, IConsoleHelpContextIds.CONSOLE_COPY_ACTION);
        setGlobalAction(actionBars, ActionFactory.COPY.getId(), action);

        // fClearOutputAction = new ClearOutputAction(fConsole);

        final ResourceBundle bundle = ConsoleResourceBundleMessages.getBundle();
        final FindReplaceAction fraction = new FindReplaceAction(bundle,
                "find_replace_action_", fConsoleView); //$NON-NLS-1$
        PlatformUI
                .getWorkbench()
                .getHelpSystem()
                .setHelp(fraction,
                        IConsoleHelpContextIds.CONSOLE_FIND_REPLACE_ACTION);
        setGlobalAction(actionBars, ActionFactory.FIND.getId(), fraction);

        fSelectionActions.add(ActionFactory.CUT.getId());
        fSelectionActions.add(ActionFactory.COPY.getId());
        fSelectionActions.add(ActionFactory.FIND.getId());

        actionBars.updateActionBars();
    }

    protected void setGlobalAction(final IActionBars actionBars,
            final String actionID, final IAction action) {
        fGlobalActions.put(actionID, action);
        actionBars.setGlobalActionHandler(actionID, action);
    }

    protected void contextMenuAboutToShow(final IMenuManager menuManager) {
        final IDocument doc = consoleOutputViewer.getDocument();
        if (doc == null) {
            return;
        }

        menuManager.add(fGlobalActions.get(ActionFactory.CUT.getId()));
        menuManager.add(fGlobalActions.get(ActionFactory.COPY.getId()));
        menuManager.add(fGlobalActions.get(ActionFactory.SELECT_ALL.getId()));

        menuManager.add(new Separator("FIND")); //$NON-NLS-1$
        menuManager.add(fGlobalActions.get(ActionFactory.FIND.getId()));
        // menuManager.add(new FollowHyperlinkAction(consoleOutputViewer));
        // menuManager.add(fClearOutputAction);

        menuManager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
    }

    protected void configureToolBar(final IToolBarManager mgr) {
        // mgr.appendToGroup(IConsoleConstants.OUTPUT_GROUP,
        // fClearOutputAction);
    }

    protected void updateSelectionDependentActions() {
        final Iterator<String> iterator = fSelectionActions.iterator();
        while (iterator.hasNext()) {
            updateAction(iterator.next());
        }
    }

    protected void updateAction(final String actionId) {
        final IAction action = fGlobalActions.get(actionId);
        if (action instanceof IUpdate) {
            ((IUpdate) action).update();
        }
    }
}
