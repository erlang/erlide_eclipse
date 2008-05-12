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
import org.erlide.ui.prefs.plugin.internal.ScrolledPageContent;
import org.osgi.service.prefs.BackingStoreException;
import org.osgi.service.prefs.Preferences;

public class SmartTypingPreferencePage extends ErlidePreferencePage implements
		IWorkbenchPreferencePage {

	public SmartTypingPreferencePage() {
		super();
		setDescription(ErlEditorMessages.SmartTypingPrefs_0);
	}

	public static final String SMART_TYPING_KEY = "smartTyping"; //$NON-NLS-1$

	private static final String SMART_TYPING_KEYS[] = new String[] {
			"strings", "atoms", "braces", "brackets", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			"parens", "pasteReindent" }; //$NON-NLS-1$

	public static int STRINGS = 0;
	public static int ATOMS = 1;
	public static int BRACES = 2;
	public static int BRACKETS = 3;
	public static int PARENS = 4;

	public static int PASTE_REINDENT = 5;

	private static final String SMART_TYPING_DEFAULTS[] = new String[] { "1",
			"0", "1", "1", "1", "1" };

	List<Button> buttons = new ArrayList<Button>();

	@Override
	protected Control createContents(Composite parent) {
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
				ErlEditorMessages.SmartTypingPrefs_2);
		addAutoclosingSection(composite);

		// composite = createSubsection(control,"Automove");
		// addAutopositionSection(composite);

		// composite = createSubsection(control, "Tabs");
		// addTabSection(composite);

		composite = createSubsection(control, "When pasting");
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

	private void addPasteSection(Composite composite) {
		buttons.add(addCheckBox(composite, "Adjust indentation"));
	}

	// private void addTabSection(Composite composite) {
	// TODO Auto-generated method stub
	//
	// }

	protected Composite createSubsection(Composite parent, String label) {
		final Group group = new Group(parent, SWT.SHADOW_NONE);
		group.setText(label);
		final GridData data = new GridData(SWT.FILL, SWT.CENTER, true, false);
		group.setLayoutData(data);
		final GridLayout layout = new GridLayout();
		layout.numColumns = 1;
		group.setLayout(layout);
		return group;
	}

	private void addAutoclosingSection(Composite composite) {
		Button b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_3);
		buttons.add(b);

		b = addCheckBox(composite, "'atoms'");
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_4);
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_5);
		buttons.add(b);

		b = addCheckBox(composite, ErlEditorMessages.SmartTypingPrefs_6);
		buttons.add(b);
	}

	private Button addCheckBox(Composite composite, String label) {
		final Button checkBox = new Button(composite, SWT.CHECK);
		checkBox.setText(label);
		return checkBox;
	}

	public void init(IWorkbench workbench) {
	}

	/*
	 * @see PreferencePage#performDefaults()
	 */
	@Override
	protected void performDefaults() {
		for (int i = 0; i < SMART_TYPING_KEYS.length; ++i) {
			buttons.get(i).setSelection(!SMART_TYPING_DEFAULTS[i].equals("0"));
		}
		super.performDefaults();
	}

	@Override
	protected void putPreferences() {
		final Preferences node = getPrefsNode();
		for (int i = 0; i < SMART_TYPING_KEYS.length; ++i) {
			final boolean b = buttons.get(i).getSelection();
			node.putBoolean(SMART_TYPING_KEY + "/" + SMART_TYPING_KEYS[i], b); //$NON-NLS-1$
		}
		try {
			node.flush();
		} catch (final BackingStoreException e) {
			e.printStackTrace();
		}
	}

	private void setToPreferences() {
		final List<String> l = getPreferences(SMART_TYPING_KEY,
				SMART_TYPING_KEYS, SMART_TYPING_DEFAULTS);
		for (int i = 0; i < l.size(); ++i) {
			final boolean b = !l.get(i).equals("0");
			buttons.get(i).setSelection(b);
		}
	}

	public static List<Boolean> getPreferences() {
		final List<String> p = getPreferences(SMART_TYPING_KEY,
				SMART_TYPING_KEYS, SMART_TYPING_DEFAULTS);
		final List<Boolean> l = new ArrayList<Boolean>(p.size());
		for (final String i : p) {
			l.add(!i.equals("0"));
		}
		return l;
	}

}
