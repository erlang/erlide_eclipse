package org.erlide.wrangler.refactoring.selection.internal;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlMember;
import org.erlide.core.erlang.IErlModule;
import org.erlide.wrangler.refactoring.backend.SyntaxInfo;
import org.erlide.wrangler.refactoring.backend.WranglerBackendManager;
import org.erlide.wrangler.refactoring.util.ErlRange;
import org.erlide.wrangler.refactoring.util.IErlRange;
import org.erlide.wrangler.refactoring.util.WranglerUtils;

public class ErlTextMemberSelection extends AbstractErlMemberSelection {

	public ErlTextMemberSelection(ITextSelection selection, ITextEditor editor) {
		IFileEditorInput input = (IFileEditorInput) editor.getEditorInput();
		document = editor.getDocumentProvider().getDocument(input);
		IFile file = input.getFile();
		store(selection, file, document);
	}

	public ErlTextMemberSelection(ITextEditor editor) {
		super(editor);
	}

	protected int getStartCol() {
		return WranglerUtils.calculateColumnFromOffset(textSelection
				.getOffset(), getStartLine() - 1, document);
	}

	protected int getEndLine() {
		return textSelection.getEndLine() + 1;
	}

	protected int getEndCol() {
		return WranglerUtils.calculateColumnFromOffset(textSelection
				.getOffset()
				+ textSelection.getLength(), getEndLine() - 1, document);
	}

	protected int getStartLine() {
		return textSelection.getStartLine() + 1;
	}

	public IErlElement getErlElement() {
		IErlModule module = (IErlModule) ErlangCore.getModel()
				.findElement(file);
		try {
			IErlElement element = module
					.getElementAt(textSelection.getOffset());
			if (element == null)
				return module;
			else
				return element;

		} catch (ErlModelException e) {
		}
		return module;
	}

	public IErlRange getMemberRange() {
		try {
			if (getErlElement() instanceof IErlMember) {
				IErlRange range = null;
				IErlMember member = (IErlMember) getErlElement();
				int sL, sC, eL, eC;
				sL = member.getLineStart() + 1;
				eL = member.getLineEnd() + 1;

				sC = WranglerUtils.calculateColumnFromOffset(member
						.getSourceRange().getOffset(), sL - 1, document);
				eC = WranglerUtils
						.calculateColumnFromOffset(member.getSourceRange()
								.getOffset()
								+ member.getSourceRange().getLength(), eL - 1,
								document);
				range = new ErlRange(sL, sC, eL, eC, member.getSourceRange()
						.getOffset(), member.getSourceRange().getLength());

				return range;
			}
		} catch (ErlModelException e) {
			e.printStackTrace();
		}
		return getSelectionRange();
	}

	public IErlRange getSelectionRange() {
		return new ErlRange(getStartLine(), getStartCol(), getEndLine(),
				getEndCol(), textSelection.getOffset(), textSelection
						.getLength());
	}

	public SelectionKind getDetailedKind() {
		if (getKind() == SelectionKind.FUNCTION
				|| getKind() == SelectionKind.FUNCTION_CLAUSE) {
			SyntaxInfo si = WranglerBackendManager.getSyntaxBackend()
					.getSyntaxInfo(file, getStartLine(), getStartCol());
			if (si.isVariable())
				return SelectionKind.VARIABLE;
			// TODO:: expression checking is not implemented
			else
				return getKind();
		} else
			return getKind();
	}
}
