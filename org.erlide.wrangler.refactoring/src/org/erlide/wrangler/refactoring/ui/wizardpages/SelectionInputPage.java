package org.erlide.wrangler.refactoring.ui.wizardpages;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.erlide.wrangler.refactoring.core.CostumWorkflowRefactoringWithPositionsSelection;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.ui.ExpressionCheckButtonListener;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

public class SelectionInputPage extends InputPage {
	CostumWorkflowRefactoringWithPositionsSelection refactoring;

	String labelText, description;

	HashMap<Button, IErlRange> checkButtons;

	Composite composite;

	Label inputLabel;

	public SelectionInputPage(String name, String description,
			String labelText,
			CostumWorkflowRefactoringWithPositionsSelection refac) {
		super(name);
		setDescription(description);
		this.labelText = labelText;
		refactoring = refac;
	}

	public void createControl(Composite parent) {
		composite = new Composite(parent, SWT.NONE);

		inputLabel = new Label(composite, SWT.LEFT);
		inputLabel.setText(labelText);
		GridData gridData = new GridData();
		gridData.horizontalAlignment = GridData.FILL;
		gridData.horizontalSpan = 2;
		inputLabel.setLayoutData(gridData);

		checkButtons = new HashMap<Button, IErlRange>();
		Button b;
		GridData gd;
		IDocument doc = ((IErlMemberSelection) GlobalParameters
				.getWranglerSelection()).getDocument();
		for (IErlRange r : refactoring.getPositions()) {
			b = new Button(composite, SWT.CHECK);
			b.setText(WranglerUtils.getTextFromEditor(r, doc) + " at "
					+ r.toString());

			gd = new GridData();
			gd.horizontalAlignment = GridData.FILL;
			gd.horizontalSpan = 2;
			gd.grabExcessHorizontalSpace = true;
			b.setLayoutData(gd);
			checkButtons.put(b, r);

			ExpressionCheckButtonListener l = new ExpressionCheckButtonListener(
					checkButtons);
			b.addMouseTrackListener(l);
		}

		GridLayout layout = new GridLayout();
		layout.numColumns = 2;
		composite.setLayout(layout);
		setControl(composite);

	}

	protected void setSelectedPositions() {
		ArrayList<IErlRange> rl = new ArrayList<IErlRange>();
		for (Entry<Button, IErlRange> e : checkButtons.entrySet()) {
			if (e.getKey().getSelection())
				rl.add(e.getValue());
		}
		refactoring.setSelectedPos(rl);
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

	@Override
	protected boolean isInputValid() {
		// TODO Auto-generated method stub
		return false;
	}

}
