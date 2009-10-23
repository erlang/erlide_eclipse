package org.erlide.ui.internal.folding;

// TODO import org.eclipse.jdt.internal.ui.text.folding.FoldingMessages;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.erlide.ui.editors.erl.folding.IErlangFoldingPreferenceBlock;

/**
 * Empty preference block for extensions to the
 * <code>org.erlide.ui.erlangFoldingStructureProvider</code> extension point
 * that do not specify their own.
 * 
 * @since 3.0
 */

public class EmptyErlangFoldingPreferenceBlock implements
		IErlangFoldingPreferenceBlock {

	// TODO fattar ju inget av detta, varf?r skapar de kontroller h?r??
	// TODO m?ste dessutom uppdatera alla @see referenser
	/*
	 * @seeorg.eclipse.jdt.internal.ui.text.folding.IJavaFoldingPreferences#
	 * createControl(org.eclipse.swt.widgets.Group)
	 */
	public Control createControl(final Composite composite) {
		final Composite inner = new Composite(composite, SWT.NONE);
		inner.setLayout(new GridLayout(3, false));

		Label label = new Label(inner, SWT.CENTER);
		GridData gd = new GridData(GridData.FILL_BOTH);
		gd.widthHint = 30;
		label.setLayoutData(gd);

		label = new Label(inner, SWT.CENTER);
		// TODO
		// label.setText(FoldingMessages.EmptyJavaFoldingPreferenceBlock_emptyCaption);
		label.setText("");
		gd = new GridData(GridData.CENTER);
		label.setLayoutData(gd);

		label = new Label(inner, SWT.CENTER);
		gd = new GridData(GridData.FILL_BOTH);
		gd.widthHint = 30;
		label.setLayoutData(gd);

		return inner;
	}

	/*
	 * @see
	 * org.eclipse.jdt.internal.ui.text.folding.IJavaFoldingPreferenceBlock#
	 * initialize()
	 */
	public void initialize() {
	}

	/*
	 * @see
	 * org.eclipse.jdt.internal.ui.text.folding.IJavaFoldingPreferenceBlock#
	 * performOk()
	 */
	public void performOk() {
	}

	/*
	 * @see
	 * org.eclipse.jdt.internal.ui.text.folding.IJavaFoldingPreferenceBlock#
	 * performDefaults()
	 */
	public void performDefaults() {
	}

	/*
	 * @see
	 * org.eclipse.jdt.internal.ui.text.folding.IJavaFoldingPreferenceBlock#
	 * dispose()
	 */
	public void dispose() {
	}

}
