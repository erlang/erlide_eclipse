/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.ui.editors.scratch;

import org.eclipse.jface.action.IAction;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.ErlideUIDebugImages;

/**
 * Stops the VM used to run a scratch.
 * 
 */
public class StopAction extends ScratchAction {

	public StopAction(final ScratchEditor editor) {
		super(editor);

		setText(ScratchMessages.getString("StopAction.label")); //$NON-NLS-1$
		setToolTipText(ScratchMessages.getString("StopAction.tooltip")); //$NON-NLS-1$
		setDescription(ScratchMessages.getString("StopAction.description")); //$NON-NLS-1$

		setImageDescriptor(ErlideUIDebugImages
				.getImageDescriptor(ErlideUIDebugImages.IMG_TOOL_TERMSNIPPET));
		setDisabledImageDescriptor(ErlideUIDebugImages
				.getImageDescriptor(ErlideUIDebugImages.IMG_TOOL_TERMSNIPPET_DISABLED));
		setHoverImageDescriptor(ErlideUIDebugImages
				.getImageDescriptor(ErlideUIDebugImages.IMG_TOOL_TERMSNIPPET_HOVER));
		// WorkbenchHelp.setHelp(this,
		// IJavaDebugHelpContextIds.TERMINATE_SCRAPBOOK_VM_ACTION);
	}

	/**
	 * @see IAction#run()
	 */
	@Override
	public void run() {
		ErlLogger.debug("stopped evaluation");
	}

	/**
	 * @see IScratchStateChangedListener#scratchStateChanged(ScratchEditor)
	 */
	public void scratchStateChanged(final ScratchEditor editor) {
		setEnabled(editor != null);
	}
}
