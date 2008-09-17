package org.erlide.wrangler.refactoring.core.foldexpression;

import java.util.HashMap;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Widget;
import org.erlide.wrangler.refactoring.core.RefactoringParameters;

import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ExpressionCheckButtonListener implements SelectionListener,
		MouseTrackListener {

	private ITextSelection selection;
	private RefactoringParameters parameters;
	private HashMap<Button, ExpressionInfo> checkButtons;

	public ExpressionCheckButtonListener(RefactoringParameters parameters,
			HashMap<Button, ExpressionInfo> checkButtons) {
		this.checkButtons = checkButtons;
		this.selection = parameters.getSelection();
		this.parameters = parameters;
	}

	// probably the SelectionListener is not needed, because the button's
	// internal state is automatically changed

	public void widgetDefaultSelected(SelectionEvent e) {
	}

	public void widgetSelected(SelectionEvent e) {
	}

	public void mouseEnter(MouseEvent e) {
		setHighlight(e.widget);
	}

	public void mouseExit(MouseEvent e) {
		resetHighlight();
	}

	public void mouseHover(MouseEvent e) {
	}

	private void setHighlight(Widget w) {
		try {
			OtpErlangTuple t = checkButtons.get(w).getStartingPos();
			int startOffset = parameters.calculateOffsetFromErlangPos(t);
			int endOffset = parameters
					.calculateOffsetFromErlangPos(checkButtons.get(w)
							.getEndingPos());
			parameters.getEditor().setHighlightRange(startOffset,
					endOffset - startOffset + 1, true);
			parameters.getEditor().selectAndReveal(startOffset,
					endOffset - startOffset + 1);
		} catch (OtpErlangRangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	private void resetHighlight() {
		parameters.getEditor().setHighlightRange(selection.getOffset(),
				selection.getLength(), true);
	}
}
