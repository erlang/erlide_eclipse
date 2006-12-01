package org.erlide.ui.actions;

import java.util.ResourceBundle;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.basicui.util.IErlangStatusConstants;
import org.erlide.core.ErlangPlugin;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.util.ResourceUtil;
import org.erlide.runtime.backend.BackendManager;
import org.erlide.runtime.backend.BackendUtil;
import org.erlide.runtime.backend.exceptions.BackendException;
import org.erlide.runtime.backend.exceptions.ErlangRpcException;
import org.erlide.ui.editors.util.EditorUtility;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

public class OpenIncludeFileAction extends TextEditorAction {

	protected String fErlModule;

	protected String fErlFunction;

	public OpenIncludeFileAction(ResourceBundle bundle, String prefix,
			ITextEditor editor) {
		super(bundle, prefix, editor);
		fErlModule = "erlide_open";
		fErlFunction = "open_included";
	}

	@Override
	public void run() {
		final ISelection sel = getTextEditor().getSelectionProvider().getSelection();
		if (sel == null || sel.isEmpty() || !(sel instanceof ITextSelection)) {
			return;
		}
		final ITextSelection selection = (ITextSelection) sel;
		final IDocument document = getTextEditor().getDocumentProvider()
				.getDocument(getTextEditor().getEditorInput());
		try {
			final int line = document.getLineOfOffset(selection.getOffset());
			final int offset = document.getLineOffset(line);
			final int length = document.getLineLength(line);
			final String text = document.get(offset, length);
			final OtpErlangObject r1 = BackendUtil.checkRpc(BackendManager
					.getDefault().getIdeBackend().rpc(fErlModule, fErlFunction,
							new OtpErlangString(text)));
			if (!(r1 instanceof OtpErlangString)) {
				final String e = r1.toString();
				ErlangPlugin.log(new Status(IStatus.ERROR,
						ErlangPlugin.PLUGIN_ID,
						IErlangStatusConstants.INTERNAL_ERROR,
						"IndentAction_error_message" + e, null));

				return;
			}
			final OtpErlangString s1 = (OtpErlangString) r1;
			final String path = s1.stringValue();
			IResource r = ResourceUtil.recursiveFindNamedResource(path);
			if (r == null) {
				r = EditorUtility.openExternal(path);
			}
			if (r != null) {
				EditorUtility.openInEditor(r);
			}
		} catch (final BadLocationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ErlangRpcException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final BackendException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final ErlModelException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (final CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	// /**
	// * Selects the given range on the editor.
	// *
	// * @param newOffset the selection offset
	// * @param newLength the selection range
	// */
	// private void selectAndReveal(int newOffset, int newLength) {
	// Assert.isTrue(newOffset >= 0);
	// Assert.isTrue(newLength >= 0);
	// ITextEditor editor= getTextEditor();
	// if (editor instanceof ErlangEditor) {
	// ErlangEditor erlEditor = ((ErlangEditor)editor);
	// erlEditor.selectAndReveal(newOffset, newLength);
	// } else
	// // this is too intrusive, but will never get called anyway
	// getTextEditor().selectAndReveal(newOffset, newLength);
	//			
	// }

}
