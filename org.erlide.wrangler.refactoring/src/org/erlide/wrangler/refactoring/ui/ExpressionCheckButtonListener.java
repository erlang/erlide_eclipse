package org.erlide.wrangler.refactoring.ui;

import java.util.HashMap;

import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackListener;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Widget;
import org.erlide.wrangler.refactoring.selection.IErlMemberSelection;
import org.erlide.wrangler.refactoring.util.GlobalParameters;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

public class ExpressionCheckButtonListener implements MouseTrackListener {

	private IErlMemberSelection selection;
	private HashMap<Button, IErlRange> checkButtons;

	public ExpressionCheckButtonListener(HashMap<Button, IErlRange> checkButtons) {
		this.checkButtons = checkButtons;
		this.selection = (IErlMemberSelection) GlobalParameters
				.getWranglerSelection();
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
		int offset = checkButtons.get(w).getOffset();
		int length = checkButtons.get(w).getLength();
		WranglerUtils.highlightSelection(offset, length, selection);

	}

	private void resetHighlight() {
		WranglerUtils.highlightSelection(selection.getSelectionRange()
				.getOffset(), selection.getSelectionRange().getLength(),
				selection);
	}
}
