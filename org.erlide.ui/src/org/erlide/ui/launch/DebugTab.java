/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.launch;

import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Tree;

public class DebugTab extends AbstractLaunchConfigurationTab {

	class TreeLabelProvider extends LabelProvider {
		@Override
		public String getText(Object element) {
			return super.getText(element);
		}

		@Override
		public Image getImage(Object element) {
			return null;
		}
	}

	class TreeContentProvider implements IStructuredContentProvider,
			ITreeContentProvider {
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		}

		public void dispose() {
		}

		public Object[] getElements(Object inputElement) {
			return getChildren(inputElement);
		}

		public Object[] getChildren(Object parentElement) {
			return new Object[] { "item_0", "item_1", "item_2" };
		}

		public Object getParent(Object element) {
			return null;
		}

		public boolean hasChildren(Object element) {
			return getChildren(element).length > 0;
		}
	}

	private Tree tree;

	public void createControl(final Composite parent) {

		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		final Group interpretedModulesGroup = new Group(comp, SWT.NONE);
		interpretedModulesGroup.setText("Interpreted modules");
		final GridData gd_interpretedModulesGroup = new GridData();
		interpretedModulesGroup.setLayoutData(gd_interpretedModulesGroup);
		interpretedModulesGroup.setLayout(new GridLayout());

		final CheckboxTreeViewer checkboxTreeViewer = new CheckboxTreeViewer(
				interpretedModulesGroup, SWT.BORDER);
		checkboxTreeViewer.setLabelProvider(new TreeLabelProvider());
		checkboxTreeViewer.setContentProvider(new TreeContentProvider());
		tree = checkboxTreeViewer.getTree();
		final GridData gd_tree = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd_tree.minimumWidth = 250;
		gd_tree.minimumHeight = 128;
		gd_tree.widthHint = 256;
		gd_tree.heightHint = 296;
		tree.setLayoutData(gd_tree);

	}

	public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
	}

	public void initializeFrom(final ILaunchConfiguration config) {
	}

	public void performApply(final ILaunchConfigurationWorkingCopy config) {
	}

	public String getName() {
		return "Debug";
	}

	@Override
	public boolean isValid(final ILaunchConfiguration config) {
		return true;
	}
}
