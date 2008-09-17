package org.erlide.wrangler.refactoring.core.foldexpression;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;
import org.erlide.wrangler.refactoring.core.WranglerRefactoring;
import org.erlide.wrangler.refactoring.ui.WranglerNewDataInputPage;

import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class FoundExpressionSelectionInputPage extends WranglerNewDataInputPage {

	HashMap<Button, ExpressionInfo> checkButtons;

	public FoundExpressionSelectionInputPage(String name) {
		super(name);
	}

	@Override
	protected String initDescription() {
		return "Folding the selected expressions";
	}

	@Override
	protected String initLabelText() {
		return "Please select the expressions would you like to be folded:";
	}

	@Override
	protected void initListeners() {
		RefactoringParameters parameters = ((FoldExpressionRefactoring) getRefactoring())
				.getParameters();
		ExpressionCheckButtonListener l = new ExpressionCheckButtonListener(
				parameters, checkButtons);
		for (Map.Entry<Button, ExpressionInfo> e : checkButtons.entrySet()) {
			e.getKey().addMouseTrackListener(l);
		}

	}

	@Override
	protected String initTitle() {
		return "To be folded expression selection";
	}

	@Override
	protected boolean performFinish() {
		setSelectedPositions();
		return super.performFinish();
	}

	@Override
	public IWizardPage getNextPage() {
		setSelectedPositions();
		return super.getNextPage();
	}

	private void setSelectedPositions() {
		ArrayList<OtpErlangTuple> selectedPositions = new ArrayList<OtpErlangTuple>();

		for (Map.Entry<Button, ExpressionInfo> e : checkButtons.entrySet()) {
			if (e.getKey().getSelection()) {
				selectedPositions.add(e.getValue().getErlangPosition());
			}
		}

		FoldExpressionRefactoring refac = (FoldExpressionRefactoring) getRefactoring();
		refac.setSelectedPositions(selectedPositions);

	}

	@Override
	protected void initExtraControls(GridLayout layout) {
		/**
		 * hopefully disables the input field
		 */
		newDataText.setVisible(false);
		setPageComplete(true);

		FoldExpressionRefactoring refac = (FoldExpressionRefactoring) getRefactoring();
		List<OtpErlangTuple> positions = refac.getFoundPositions();
		checkButtons = new HashMap<Button, ExpressionInfo>();

		ExpressionInfo i;
		Button b;
		GridData gridData = new GridData();
		RefactoringParameters param = ((WranglerRefactoring) getRefactoring())
				.getParameters();
		try {
			for (OtpErlangTuple t : positions) {
				b = new Button(composite, SWT.CHECK);
				i = new ExpressionInfo(t);

				b.setText(param.getTextFromEditor(i.getStartingPos(), i
						.getEndingPos())
						+ " at "
						+ i.getStartingPos().toString()
						+ " - "
						+ i.getEndingPos().toString());
				gridData.horizontalAlignment = GridData.FILL;
				gridData.horizontalSpan = 2;
				gridData.grabExcessHorizontalSpace = true;
				b.setLayoutData(gridData);
				checkButtons.put(b, i);
			}
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}
}
