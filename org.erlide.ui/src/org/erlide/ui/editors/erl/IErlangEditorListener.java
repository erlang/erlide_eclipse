package org.erlide.ui.editors.erl;

import java.util.ListResourceBundle;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.erlide.ui.util.ErlideSelection;

public interface IErlangEditorListener {

    /**
     * Anytime an ErlangEditor is saved, it will notify that to its listeners.
     * 
     * @param edit
     *            the PyEdit that has just been saved.
     */
    void onSave(ErlangEditor edit, IProgressMonitor monitor);

    /**
     * When the actions are being created in ErlangEditor, this method is
     * called, so that contributors might add their own actions
     * 
     * @param resources
     *            the resource bundle it used
     * @param edit
     *            the PyEdit
     */
    void onCreateActions(ListResourceBundle resources, ErlangEditor edit,
            IProgressMonitor monitor);

    /**
     * This method is called whenever the editor is disposed
     * 
     * @param edit
     *            the edit that will be disposed.
     */
    void onDispose(ErlangEditor edit, IProgressMonitor monitor);

    /**
     * Use to notify listeners that the document that the editor was editing has
     * just changed.
     * 
     * @param document
     *            the document being edited
     * @param edit
     *            the editor that had the document changed
     * @param monitor
     *            the monitor for the change
     */
    void onSetDocument(IDocument document, ErlangEditor edit,
            IProgressMonitor monitor);

    /**
     * Called when the cursor position changes.
     * 
     * Note: the listeners of this method should be very efficient, as in any
     * change, it will be called.
     * 
     * @param edit
     *            the editor that had its cursor position changed.
     * @param ps
     *            the new selection (after the cursor changed its position)
     */
    void handleCursorPositionChanged(ErlangEditor edit, ErlideSelection ps);

    /**
     * Called when the input of the editor is changed.
     * 
     * @param edit
     *            the editor that had the input changed
     * @param oldInput
     *            the old input of the editor
     * @param input
     *            the new input of the editor
     * @param monitor
     *            the monitor for the job that's making the notifications
     */
    void onInputChanged(ErlangEditor edit, IEditorInput oldInput,
            IEditorInput input, IProgressMonitor monitor);

}
