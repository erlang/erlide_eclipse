package org.erlide.ui.prefs.plugin;

import java.util.ArrayList;
import java.util.List;

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
import org.erlide.ui.ErlideUIPlugin;
import org.erlide.runtime.ErlLogger;
import org.erlide.ui.prefs.plugin.internal.ScrolledPageContent;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

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

	public static final int STRINGS = 0;
	public static final int ATOMS = 1;
	public static final int BRACES = 2;
	public static final int BRACKETS = 3;
	public static final int PARENS = 4;
	public static final int EMBRACE_SELECTION = 5;

	public static final int PASTE_REINDENT = 6;

	private static final String SMART_TYPING_DEFAULTS[] = new String[] { "1", //$NON-NLS-1$
			"0", "1", "1", "1", "1", "1" }; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$

	List<Button> buttons = new ArrayList<Button>();

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

		scrolled.setContent(control);
		final Point size = control.computeSize(SWT.DEFAULT, SWT.DEFAULT);
		scrolled.setMinSize(size.x, size.y);

		setToPreferences();

		return scrolled;
	}

	// private void addStringsSection(Composite composite) {
	// TODO Auto-generated method stub
	//
	// }

	private void addPasteSection(final Composite composite) {
		buttons.add(addCheckBox(composite,
				ErlEditorMessages.SmartTypinngPrefs_AdjustIndentation));
	}

	// private void addTabSection(Composite composite) {
	// TODO Auto-generated method stub
	//
	// }

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
		Button b = addCheckBox(composite,
				ErlEditorMessages.SmartTypingPrefs_Strings);
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_atoms);
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_Braces);
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_Brackets);
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_Parens);
		buttons.add(b);

		b = addCheckBox(composite,
				ErlEditorMessages.SmartTypingPrefs_EmbraceSelection);
		buttons.add(b);
	}

	private Button addCheckBox(final Composite composite, final String label) {
		final Button checkBox = new Button(composite, SWT.CHECK);
		checkBox.setText(label);
		return checkBox;
	}

	public void init(final IWorkbench workbench) {
	}

	/*
	 * @see PreferencePage#performDefaults()
	 */
	@Override
	protected void performDefaults() {
		for (int i = 0; i < SMART_TYPING_KEYS.length; ++i) {
			buttons.get(i).setSelection(!SMART_TYPING_DEFAULTS[i].equals("0")); //$NON-NLS-1$
		}
		super.performDefaults();
	}

	@Override
	protected void putPreferences() {
		final Preferences node = ErlideUIPlugin.getPrefsNode();
		for (int i = 0; i < SMART_TYPING_KEYS.length; ++i) {
			final boolean b = buttons.get(i).getSelection();
			node.putBoolean(SMART_TYPING_KEY + "/" + SMART_TYPING_KEYS[i], b); //$NON-NLS-1$
		}
		try {
			node.flush();
		} catch (final BackingStoreException e) {
			ErlLogger.warn(e);
		}
	}

	@SuppressWarnings("boxing")
	private void setToPreferences() {
		final List<Boolean> l = getPreferences();
		for (int i = 0; i < l.size(); ++i) {
			final boolean b = l.get(i);
			buttons.get(i).setSelection(b);
		}
	}

	@SuppressWarnings("boxing")
	public static List<Boolean> getPreferences() {
		final List<String> p = getPreferences(SMART_TYPING_KEY,
				SMART_TYPING_KEYS, SMART_TYPING_DEFAULTS);
		final List<Boolean> l = new ArrayList<Boolean>(p.size());
		for (final String i : p) {
			l.add(!i.equals("0") && !i.equals("false")); //$NON-NLS-1$ //$NON-NLS-2$
		}
		return l;
	}

}
