package org.erlide.ui.editors.erl.autoedit;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.erlide.ui.prefs.plugin.ErlEditorMessages;
import org.erlide.ui.prefs.plugin.ErlidePreferencePage;
import org.erlide.ui.prefs.plugin.internal.ScrolledPageContent;

public class SmartTypingPreferencePage extends ErlidePreferencePage implements
        IWorkbenchPreferencePage {

    public SmartTypingPreferencePage() {
        super();
        setDescription(ErlEditorMessages.SmartTypingPrefs_Desc);
    }

    public static final String SMART_TYPING_KEY = "smartTyping"; //$NON-NLS-1$

    private static final String SMART_TYPING_KEYS[] = new String[] {
            "strings", "atoms", "braces", "brackets", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
            "parens", "embraceSelection", "pasteReindent" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

    public static final String AUTO_NL_KEY = "indentation"; //$NON-NLS-1$
    public static final String AUTO_NL_KEYS[] = {
            "semicolon_nl", "dot_nl", "arrow_nl", "comma_nl" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    public static final int STRINGS = 0;
    public static final int ATOMS = 1;
    public static final int BRACES = 2;
    public static final int BRACKETS = 3;
    public static final int PARENS = 4;
    public static final int EMBRACE_SELECTION = 5;

    public static final int PASTE_REINDENT = 6;

    private static final String SMART_TYPING_DEFAULTS[] = new String[] { "1", //$NON-NLS-1$
            "0", "1", "1", "1", "1", "1" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

    private static final String[] AUTO_NL_DEFAULTS = new String[] { "0", "0", //$NON-NLS-1$ //$NON-NLS-2$
            "0", "0" }; //$NON-NLS-1$ //$NON-NLS-2$

    private final List<Button> buttons = new ArrayList<Button>();
    private final List<Button> autoNLButtons = new ArrayList<Button>();

    @Override
    protected Control createContents(final Composite parent) {
        final ScrolledPageContent scrolled = new ScrolledPageContent(parent,
                SWT.H_SCROLL | SWT.V_SCROLL);
        scrolled.setExpandHorizontal(true);
        scrolled.setExpandVertical(true);

        final Composite control = new Composite(scrolled, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        control.setLayout(layout);

        Composite composite;

        composite = createSubsection(control,
                ErlEditorMessages.SmartTypingPrefs_AutomaticallyClose);
        addAutoclosingSection(composite);

        // composite = createSubsection(control,"Automove");
        // addAutopositionSection(composite);

        // composite = createSubsection(control, "Tabs");
        // addTabSection(composite);

        composite = createSubsection(control,
                ErlEditorMessages.SmartTypingPrefs_WhenPasting);
        addPasteSection(composite);

        // composite = createSubsection(control, "strings");
        // addStringsSection(composite);

        composite = createSubsection(control,
                ErlEditorMessages.SmartTypingPrefs_autoNewLine);
        addAutoNLSection(composite);

        scrolled.setContent(control);
        final Point size = control.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        scrolled.setMinSize(size.x, size.y);

        setToPreferences();

        return scrolled;
    }

    private void addPasteSection(final Composite composite) {
        buttons.add(addCheckBox(composite,
                ErlEditorMessages.SmartTypingPrefs_AdjustIndentation));
    }

    protected Composite createSubsection(final Composite parent,
            final String label) {
        final Group group = new Group(parent, SWT.SHADOW_NONE);
        group.setText(label);
        final GridData data = new GridData(SWT.FILL, SWT.CENTER, true, false);
        group.setLayoutData(data);
        final GridLayout layout = new GridLayout();
        layout.numColumns = 1;
        group.setLayout(layout);
        return group;
    }

    private void addAutoclosingSection(final Composite composite) {
        final String strings[] = { ErlEditorMessages.SmartTypingPrefs_Strings,
                ErlEditorMessages.SmartTypingPrefs_atoms,
                ErlEditorMessages.SmartTypingPrefs_Braces,
                ErlEditorMessages.SmartTypingPrefs_Brackets,
                ErlEditorMessages.SmartTypingPrefs_Parens,
                ErlEditorMessages.SmartTypingPrefs_EmbraceSelection };
        addCheckboxes(composite, strings, buttons);
    }

    private void addAutoNLSection(final Composite composite) {
        final String[] nlStrings = { ErlEditorMessages.Prefs_Semicolon_nl,
                ErlEditorMessages.Prefs_Dot_nl,
                ErlEditorMessages.Prefs_Arrow_nl,
                ErlEditorMessages.Prefs_Comma_nl };
        addCheckboxes(composite, nlStrings, autoNLButtons);
    }

    @Override
    public void init(final IWorkbench workbench) {
    }

    /*
     * @see PreferencePage#performDefaults()
     */
    @Override
    protected void performDefaults() {
        setToDefaults(SMART_TYPING_KEYS, SMART_TYPING_DEFAULTS, buttons);
        setToDefaults(AUTO_NL_KEYS, AUTO_NL_DEFAULTS, autoNLButtons);
        super.performDefaults();
    }

    @Override
    protected void putPreferences() {
        putPreferences(SMART_TYPING_KEY, SMART_TYPING_KEYS, buttons);
        putPreferences(AUTO_NL_KEY, AUTO_NL_KEYS, autoNLButtons);
    }

    private void setToPreferences() {
        setToPreferences(SMART_TYPING_KEY, SMART_TYPING_KEYS,
                SMART_TYPING_DEFAULTS, buttons);
        setToPreferences(AUTO_NL_KEY, AUTO_NL_KEYS, AUTO_NL_DEFAULTS,
                autoNLButtons);
    }

    public static void addAutoNLKeysAndPrefs(final Map<String, String> prefs) {
        addKeysAndPrefs(AUTO_NL_KEY, AUTO_NL_KEYS, AUTO_NL_DEFAULTS, prefs);
    }

    public static List<Boolean> getBracketInserterPreferences() {
        return getBooleanPreferences(getPreferences(SMART_TYPING_KEY,
                SMART_TYPING_KEYS, SMART_TYPING_DEFAULTS));
    }
}
