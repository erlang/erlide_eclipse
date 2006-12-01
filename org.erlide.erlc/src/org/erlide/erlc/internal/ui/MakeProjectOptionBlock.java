/*******************************************************************************
 * Copyright (c) 2000, 2005 QNX Software Systems and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     QNX Software Systems - Initial API and implementation
 *******************************************************************************/
package org.erlide.erlc.internal.ui;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.erlide.erlc.ErlideErlcPlugin;
import org.erlide.erlc.builder.ErlcMakeBuilder;
import org.erlide.erlc.ui.dialogs.IErlOptionContainer;
import org.erlide.erlc.ui.dialogs.MakeEnvironmentBlock;
import org.erlide.erlc.ui.dialogs.SettingsBlock;
import org.erlide.erlc.ui.dialogs.TabFolderOptionBlock;

public class MakeProjectOptionBlock extends TabFolderOptionBlock {

	// private IErlOptionContainer optionContainer;
	public MakeProjectOptionBlock() {
		super(true);
	}

	public MakeProjectOptionBlock(IErlOptionContainer parent) {
		super(parent);
		// optionContainer = parent;
	}

	@Override
	protected void addTabs() {
		addTab(new SettingsBlock(ErlideErlcPlugin.getDefault()
				.getPluginPreferences(), ErlcMakeBuilder.BUILDER_ID));
		addTab(new MakeEnvironmentBlock(ErlideErlcPlugin.getDefault()
				.getPluginPreferences(), ErlcMakeBuilder.BUILDER_ID));
		// addTab(new DiscoveryOptionsBlock());
	}

	@Override
	public void setOptionContainer(IErlOptionContainer parent) {
		super.setOptionContainer(parent);
		// optionContainer = parent;
	}

	@Override
	public Control createContents(Composite parent) {
		final Control control = super.createContents(parent);
		// FIXME help system
		// List optionPages = getOptionPages();
		// Iterator iter = optionPages.iterator();
		// for( int i = 0; i < 4 && iter.hasNext(); i++ ){
		// IErlOptionPage page = (IErlOptionPage) iter.next();
		// IWorkbenchHelpSystem helpSystem =
		// ErlideErlcPlugin.getDefault().getWorkbench().getHelpSystem();
		// if( optionContainer != null && optionContainer instanceof
		// MakePropertyPage )
		// switch( i ){
		// case 0 : helpSystem.setHelp(page.getControl(),
		// IMakeHelpContextIds.MAKE_PROP_BUILDER_SETTINGS); break;
		// case 1 : helpSystem.setHelp(page.getControl(),
		// IMakeHelpContextIds.MAKE_PROP_DISCOVERY ); break;
		// }
		// else
		// switch( i ){
		// case 0 : helpSystem.setHelp(page.getControl(),
		// IMakeHelpContextIds.MAKE_BUILDER_SETTINGS); break;
		// case 1 : helpSystem.setHelp(page.getControl(),
		// IMakeHelpContextIds.SCANNER_CONFIG_DISCOVERY_OPTIONS ); break;
		// }
		// }
		//
		return control;
	}
}
