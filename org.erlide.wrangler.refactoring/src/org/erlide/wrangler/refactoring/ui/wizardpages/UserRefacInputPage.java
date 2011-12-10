package org.erlide.wrangler.refactoring.ui.wizardpages;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.erlide.wrangler.refactoring.core.internal.UserRefactoring;
import org.erlide.wrangler.refactoring.ui.validator.IValidator;

/**
 * Page for providing arguments to user defined refactorings
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * @version %I%, %G%
 */
public class UserRefacInputPage extends InputPage {

    protected String inputErrorMsg;

    IValidator validator;
    protected List<Label> inputLabels;
    protected List<Text> inputTexts;

    private final String description;

    private final ModifyListener modifyListener = new ModifyListener() {

        @Override
        public void modifyText(final ModifyEvent e) {
            isInputValid();
        }

    };

    /**
     * Constructor
     * 
     * @param name
     *            Refactoring name (title)
     * @param description
     *            description
     * @param labelText
     *            input label's text
     * @param inputErrorMsg
     *            error message in case of wrong input
     * @param validator
     *            validator object
     */
    public UserRefacInputPage(final String name, final String description,
            final String inputErrorMsg, final IValidator validator) {
        super(name);
        setDescription(description);
        this.inputErrorMsg = inputErrorMsg;
        this.validator = validator;
        this.description = description;
        setPageComplete(false);

    }

    @Override
    public void createControl(final Composite parent) {

        final Composite composite = new Composite(parent, SWT.NONE);

        final GridLayout layout = new GridLayout();
        layout.numColumns = 3;
        composite.setLayout(layout);

        final List<String> parPrompts = ((UserRefactoring) getRefactoring())
                .getParPrompts();

        if (parPrompts.size() == 0) {
            final Label label = new Label(composite, SWT.LEFT);
            label.setText("No arguments for this refactoring");
            setControl(composite);
            setPageComplete(true);
            return;
        }

        final GridData lgData = new GridData();
        lgData.horizontalAlignment = GridData.FILL;
        lgData.horizontalSpan = 3;
        final Label descrLabel = new Label(composite, SWT.LEFT);
        descrLabel.setText(description);
        descrLabel.setLayoutData(lgData);

        inputLabels = new ArrayList<Label>(parPrompts.size());
        inputTexts = new ArrayList<Text>(parPrompts.size());

        for (final String labelText : parPrompts) {
            GridData gridData = new GridData();
            gridData.horizontalAlignment = GridData.FILL;
            gridData.horizontalSpan = 1;
            final Label label = new Label(composite, SWT.LEFT);
            label.setText(labelText);
            label.setLayoutData(gridData);
            inputLabels.add(label);

            gridData = new GridData();
            gridData.horizontalAlignment = GridData.FILL;
            gridData.horizontalSpan = 2;
            gridData.grabExcessHorizontalSpace = true;
            final Text text = new Text(composite, SWT.NONE);
            text.setLayoutData(gridData);
            text.addModifyListener(modifyListener);
            inputTexts.add(text);
        }

        setControl(composite);

        inputTexts.get(0).setFocus();

    }

    @Override
    protected void controlWorkflow(final Shell s) {
        ((UserRefactoring) getRefactoring()).getWorkflowController(s)
                .doRefactoring();
    }

    @Override
    protected boolean isInputValid() {
        if (checkCorrectness()) {
            final List<String> params = new ArrayList<String>(inputTexts.size());
            for (final Text text : inputTexts) {
                params.add(text.getText());
            }
            ((UserRefactoring) getRefactoring()).setParValue(params);
            setErrorMessage(null);
            setPageComplete(true);
            return true;
        }
        setPageComplete(false);
        setErrorMessage(inputErrorMsg);
        return false;
    }

    private boolean checkCorrectness() {
        for (final Text text : inputTexts) {
            if (!validator.isValid(text.getText())) {
                return false;
            }
        }
        return true;
    }

}
