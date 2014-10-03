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
import java.util.List;
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
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.IFindReplaceTarget;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CaretEvent;
import org.eclipse.swt.custom.CaretListener;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.StyledText;
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
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.internal.console.ConsoleResourceBundleMessages;
import org.eclipse.ui.internal.console.IConsoleHelpContextIds;
import org.eclipse.ui.part.Page;
import org.eclipse.ui.texteditor.FindReplaceAction;
import org.eclipse.ui.texteditor.IUpdate;
import org.erlide.backend.api.IBackend;
import org.erlide.engine.services.parsing.RuntimeHelper;
import org.erlide.runtime.api.ParserException;
import org.erlide.runtime.shell.IBackendShell;
import org.erlide.ui.editors.erl.ColorManager;
import org.erlide.ui.internal.ErlideUIPlugin;
import org.erlide.ui.util.IColorManager;
import org.erlide.util.StringUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

@SuppressWarnings("restriction")
public class ErlangConsolePage extends Page implements IAdaptable,
        IPropertyChangeListener, IErlangConsolePage {
    public static final String ID = "org.erlide.ui.views.console";

    Color bgColor_Ok;
    Color bgColor_AlmostOk;
    Color bgColor_Err;

    StyledText consoleOutputText;
    private final ErlConsoleDocument fDoc;
    final ErlangConsoleHistory history = new ErlangConsoleHistory();
    StyledText consoleInputText;
    SourceViewer consoleOutputViewer;
    private SourceViewer consoleInputViewer;
    private IBackendShell shell;
    protected Map<String, IAction> fGlobalActions = new HashMap<String, IAction>();
    protected List<String> fSelectionActions = new ArrayList<String>();
    // protected ClearOutputAction fClearOutputAction;
    private final ErlangConsole fConsole;
    private IConsoleView fConsoleView;
    private MenuManager fMenuManager;
    private Composite composite;
    private boolean disposeColors;
    private final IBackend backend;

    private final ISelectionChangedListener selectionChangedListener = new ISelectionChangedListener() {
        @Override
        public void selectionChanged(final SelectionChangedEvent event) {
            updateSelectionDependentActions();
        }
    };

    private Color bgcolor;

    public ErlangConsolePage(final IConsoleView view, final ErlangConsole console,
            final IBackend backend) {
        super();
        fConsole = console;
        fConsoleView = view;
        shell = console.getShell();
        fDoc = new ErlConsoleDocument(shell);
        this.backend = backend;
    }

    @Override
    public void dispose() {
        consoleOutputViewer.getSelectionProvider().removeSelectionChangedListener(
                selectionChangedListener);
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
            bgColor_AlmostOk.dispose();
        }
        ErlideUIPlugin.getDefault().getErlConsoleManager().removePage(fConsole);
        super.dispose();
    }

    boolean isInputComplete() {
        if (!backend.getRuntime().isRunning()) {
            return false;
        }
        try {
            final String str = consoleInputText.getText() + " ";
            final RuntimeHelper helper = new RuntimeHelper(backend.getOtpRpc());
            final OtpErlangObject o = helper.parseConsoleInput(str);
            if (o instanceof OtpErlangList && ((OtpErlangList) o).arity() == 0) {
                return false;
            }
            if (!(o instanceof OtpErlangList)) {
                return false;
            }
        } catch (final ParserException e) {
            return false;
        }
        return true;
    }

    protected void sendInput() {
        final String s = consoleInputText.getText();
        input(s);
        consoleInputText.setText("");
    }

    @Override
    public void input(final String data) {
        final String data2 = data.trim() + "\n";
        shell.input(data2);
        shell.send(data2);
        history.addToHistory(data.trim());
    }

    public void setInput(final String str) {
        consoleInputText.setText(str);
        consoleInputText.setSelection(str.length());
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
        consoleOutputText = consoleOutputViewer.getTextWidget();
        consoleOutputText.setFont(JFaceResources.getTextFont());
        bgcolor = DebugUIPlugin
                .getPreferenceColor(IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR);
        consoleOutputText.setBackground(bgcolor);
        DebugUIPlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new IPropertyChangeListener() {
                    @Override
                    public void propertyChange(final PropertyChangeEvent event) {
                        if (event.getProperty().equals(
                                IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR)) {
                            final Color color = DebugUIPlugin
                                    .getPreferenceColor(IDebugPreferenceConstants.CONSOLE_BAKGROUND_COLOR);
                            consoleOutputText.setBackground(color);
                            consoleInputText.setBackground(color);
                        }
                    }
                });
        consoleOutputText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                if (e.stateMask == 0 && e.character != '\0') {
                    consoleInputText.setFocus();
                    consoleInputText.append("" + e.character);
                    consoleInputText.setCaretOffset(consoleInputText.getText().length());
                }
                e.doit = true;
            }

        });

        final IPreferenceStore store = ErlideUIPlugin.getDefault().getPreferenceStore();
        final IColorManager colorManager = new ColorManager();
        consoleOutputViewer.setDocument(fDoc);
        consoleOutputViewer.configure(new ErlangConsoleSourceViewerConfiguration(store,
                colorManager, backend));

        consoleInputViewer = new SourceViewer(sashForm, null, SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        consoleInputText = consoleInputViewer.getTextWidget();
        consoleInputViewer.setDocument(new Document());
        consoleInputViewer.configure(new ErlangConsoleSourceViewerConfiguration(store,
                colorManager, backend));

        sashForm.setWeights(new int[] { 2, 1 });

        final Label helpLabel = new Label(composite, SWT.NONE);
        helpLabel
                .setText("To send the input to the console: press Enter at the end of an expression."
                        + "Ctrl/Cmd-arrows navigate the input history.");
        helpLabel.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

        final ModifyListener modifyListener = new ModifyListener() {
            @Override
            public void modifyText(final ModifyEvent e) {
                final String consoleText = trimInput(consoleInputText.getText());
                final boolean atEndOfInput = consoleText.endsWith(".")
                        && consoleInputText.getCaretOffset() >= consoleText.length();

                if (atEndOfInput) {
                    final boolean inputComplete = isInputComplete();
                    if (inputComplete) {
                        consoleInputText.setBackground(bgColor_Ok);
                    }
                } else {
                    consoleInputText.setBackground(bgcolor);
                }
            }
        };
        // consoleInput.addModifyListener(modifyListener);
        consoleInputText.addCaretListener(new CaretListener() {
            @Override
            public void caretMoved(final CaretEvent event) {
                modifyListener.modifyText(null);
            }
        });
        consoleInputText.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(final KeyEvent e) {
                final boolean ctrlOrCommandPressed = (e.stateMask & SWT.MOD1) == SWT.MOD1;
                final String conText = trimInput(consoleInputText.getText());
                final boolean atEndOfInput = consoleInputText.getCaretOffset() >= conText
                        .length() && conText.endsWith(".");
                e.doit = true;

                if (e.keyCode == 13 && (ctrlOrCommandPressed || atEndOfInput)) {
                    final boolean inputComplete = isInputComplete();
                    if (inputComplete) {
                        sendInput();
                    }
                } else if (ctrlOrCommandPressed && e.keyCode == SWT.SPACE) {
                    consoleInputViewer.doOperation(ISourceViewer.CONTENTASSIST_PROPOSALS);
                } else if (ctrlOrCommandPressed && e.keyCode == SWT.ARROW_UP) {
                    e.doit = false;
                    history.prev();
                    final String s = history.get();
                    if (s != null) {
                        consoleInputText.setText(s);
                        consoleInputText
                                .setSelection(consoleInputText.getText().length());
                    }
                } else if (ctrlOrCommandPressed && e.keyCode == SWT.ARROW_DOWN) {
                    e.doit = false;
                    history.next();
                    final String s = history.get();
                    if (s != null) {
                        consoleInputText.setText(s);
                        consoleInputText
                                .setSelection(consoleInputText.getText().length());
                    }
                }
            }

        });
        consoleInputText.setFont(consoleOutputText.getFont());
        consoleInputText.setBackground(consoleOutputText.getBackground());
        consoleInputText.setWordWrap(true);
        consoleInputText.setFocus();

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
        addDocumentListener(documentListener);

        final String id = "#ContextMenu"; //$NON-NLS-1$
        // if (getConsole().getType() != null) {
        //            id = getConsole().getType() + "." + id; //$NON-NLS-1$
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
        java.awt.Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(),
                hsbvals);

        if (hsbvals[1] >= 0.01) {
            bgColor_Ok = color;
            bgColor_AlmostOk = color;
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

            rgb = java.awt.Color.HSBtoRGB(green, 2 * deltaSaturation, hsbvals[2]);
            cx = new java.awt.Color(rgb);
            bgColor_Ok = new Color(Display.getCurrent(), new RGB(cx.getRed(),
                    cx.getGreen(), cx.getBlue()));

            rgb = java.awt.Color.HSBtoRGB(green, deltaSaturation, hsbvals[2]);
            cx = new java.awt.Color(rgb);
            bgColor_AlmostOk = new Color(Display.getCurrent(), new RGB(cx.getRed(),
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
        consoleInputText.setFocus();
    }

    @Override
    public Object getAdapter(final Class required) {
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

            if (source.equals(fConsole) && IConsoleConstants.P_FONT.equals(property)) {
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
        // TextViewerAction action = new TextViewerAction(consoleOutputViewer,
        // ITextOperationTarget.SELECT_ALL);
        // action.configureAction(ConsoleMessages.TextConsolePage_SelectAllText,
        // ConsoleMessages.TextConsolePage_SelectAllDescrip,
        // ConsoleMessages.TextConsolePage_SelectAllDescrip);
        // action.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_SELECT_ALL);
        // PlatformUI.getWorkbench().getHelpSystem()
        // .setHelp(action, IConsoleHelpContextIds.CONSOLE_SELECT_ALL_ACTION);
        // setGlobalAction(actionBars, ActionFactory.SELECT_ALL.getId(),
        // action);
        //
        // action = new TextViewerAction(consoleOutputViewer,
        // ITextOperationTarget.CUT);
        // action.configureAction(ConsoleMessages.TextConsolePage_CutText,
        // ConsoleMessages.TextConsolePage_CutDescrip,
        // ConsoleMessages.TextConsolePage_CutDescrip);
        // action.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
        // .getImageDescriptor(ISharedImages.IMG_TOOL_CUT));
        // action.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_CUT);
        // PlatformUI.getWorkbench().getHelpSystem()
        // .setHelp(action, IConsoleHelpContextIds.CONSOLE_CUT_ACTION);
        // setGlobalAction(actionBars, ActionFactory.CUT.getId(), action);
        //
        // action = new TextViewerAction(consoleOutputViewer,
        // ITextOperationTarget.COPY);
        // action.configureAction(ConsoleMessages.TextConsolePage_CopyText,
        // ConsoleMessages.TextConsolePage_CopyDescrip,
        // ConsoleMessages.TextConsolePage_CopyDescrip);
        // action.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
        // .getImageDescriptor(ISharedImages.IMG_TOOL_COPY));
        // action.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_COPY);
        // PlatformUI.getWorkbench().getHelpSystem()
        // .setHelp(action, IConsoleHelpContextIds.CONSOLE_COPY_ACTION);
        // setGlobalAction(actionBars, ActionFactory.COPY.getId(), action);

        // fClearOutputAction = new ClearOutputAction(fConsole);

        final ResourceBundle bundle = ConsoleResourceBundleMessages.getBundle();
        final FindReplaceAction fraction = new FindReplaceAction(bundle,
                "find_replace_action_", fConsoleView); //$NON-NLS-1$
        PlatformUI.getWorkbench().getHelpSystem()
                .setHelp(fraction, IConsoleHelpContextIds.CONSOLE_FIND_REPLACE_ACTION);
        setGlobalAction(actionBars, ActionFactory.FIND.getId(), fraction);

        // fSelectionActions.add(ActionFactory.CUT.getId());
        // fSelectionActions.add(ActionFactory.COPY.getId());
        fSelectionActions.add(ActionFactory.FIND.getId());

        actionBars.updateActionBars();
    }

    protected void setGlobalAction(final IActionBars actionBars, final String actionID,
            final IAction action) {
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

    public void addDocumentListener(final IDocumentListener documentListener) {
        fDoc.addDocumentListener(documentListener);
    }

    public void removeDocumentListener(final IDocumentListener documentListener) {
        fDoc.removeDocumentListener(documentListener);
    }

    public IBackendShell getShell() {
        return shell;
    }

    private static String trimInput(final String s) {
        String conText = StringUtils.rightTrim(s, ' ');
        conText = StringUtils.rightTrim(conText, '\n');
        conText = StringUtils.rightTrim(conText, '\r');
        return conText;
    }
}
