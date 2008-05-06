/**
 * 
 */
package org.erlide.ui.editors.erl.test;

import java.util.ResourceBundle;

import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.erlide.core.erlang.IErlModule;

/**
 * @author jakob
 * 
 */
public class TestAction extends TextEditorAction {

	private final IErlModule module;

	public TestAction(ResourceBundle bundle, String prefix, ITextEditor editor,
			IErlModule module) {
		super(bundle, prefix, editor);
		this.module = module;
	}

}
