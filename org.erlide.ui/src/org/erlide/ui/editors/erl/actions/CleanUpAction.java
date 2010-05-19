/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Alain O'Dea
 *******************************************************************************/
package org.erlide.ui.editors.erl.actions;

import org.eclipse.core.resources.IResource;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.IWorkbenchSite;
import org.erlide.core.cleanup.CleanUpProviders;
import org.erlide.core.erlang.IErlModule;
import org.erlide.jinterface.util.ErlLogger;
import org.erlide.ui.editors.erl.ErlangEditor;

/*******************************************************************************
 * <p>
 * The {@link CleanUpAction} class provides the {@link Action} behind Erlang
 * module source code clean up.
 * </p>
 * 
 * @author Alain O'Dea [alain dot odea at gmail dot com]
 *******************************************************************************/
public class CleanUpAction extends Action {

	private final IWorkbenchSite site;

	public CleanUpAction(final IWorkbenchSite site) {
		super("Clean Up...");
		this.site = site;
	}

	@Override
	public void run() {
		try {
			final ErlangEditor editor = (ErlangEditor) getSite().getPage()
					.getActiveEditor();
			final IErlModule module = editor.getModule();
			if (module == null) {
				return;
			}
			final IResource resource = module.getResource();
			if (resource == null) {
				return;
			}
			CleanUpProviders.createForIResource(resource).cleanUp();
		} catch (Exception e) {
			ErlLogger.debug(e);
		}
	}

	private IWorkbenchSite getSite() {
		return site;
	}

}
