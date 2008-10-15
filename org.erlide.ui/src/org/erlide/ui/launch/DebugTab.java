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

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.runtime.backend.IErlLaunchAttributes;
import org.erlide.runtime.debug.IErlDebugConstants;
import org.erlide.ui.util.SWTUtil;

public class DebugTab extends AbstractLaunchConfigurationTab {

	CheckboxTreeViewer checkboxTreeViewer;
	private Button attachOnFirstCallCheck;
	private Button attachOnBreakpointCheck;
	private Button attachOnExitCheck;
	private Button distributedDebugCheck;
	List<IErlModule> interpretedModules;

	class TreeLabelProvider extends LabelProvider {
		@Override
		public String getText(Object element) {
			if (element instanceof DebugTreeItem) {
				return ((DebugTreeItem) element).item.getName();
			}
			return super.getText(element);
		}

		@Override
		public Image getImage(Object element) {
			return null;
		}
	}

	class DebugTreeItem {
		IErlElement item = null;
		DebugTreeItem parent = null;
		List<DebugTreeItem> children = new ArrayList<DebugTreeItem>();

		boolean isFullyChecked() {
			for (DebugTreeItem i : children) {
				if (!interpretedModules.contains(i.item)) {
					return false;
				}
			}
			return true;
		}

		boolean isFullyUnchecked() {
			for (DebugTreeItem i : children) {
				if (interpretedModules.contains(i.item)) {
					return false;
				}
			}
			return true;
		}

	}

	class TreeContentProvider implements IStructuredContentProvider,
			ITreeContentProvider {
		String[] projects;
		ILaunchConfiguration input;
		DebugTreeItem root;

		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			String projs;
			if (newInput instanceof ILaunchConfiguration) {
				input = (ILaunchConfiguration) newInput;
				root = new DebugTreeItem();
				try {
					projs = input.getAttribute(IErlLaunchAttributes.PROJECTS,
							"").trim();
				} catch (final CoreException e1) {
					projs = "";
				}
				projects = projs.length() == 0 ? new String[] {} : projs
						.split(";");
				for (String p : projects) {
					IErlProject pj = ErlangCore.getModel().getErlangProject(p);
					DebugTreeItem m = new DebugTreeItem();
					m.item = pj;
					root.children.add(m);

					try {
						List<IErlModule> ms = pj.getModules();
						for (IErlModule mm : ms) {
							if (mm.getModuleKind() == ModuleKind.ERL) {
								DebugTreeItem mi = new DebugTreeItem();
								mi.item = mm;
								mi.parent = m;
								m.children.add(mi);
							}
						}
					} catch (ErlModelException e) {
						e.printStackTrace();
					}

				}
			} else {
				projects = null;
				root = new DebugTreeItem();
			}
		}

		public void dispose() {
		}

		public Object[] getElements(Object inputElement) {
			return root.children.toArray();
		}

		public Object[] getChildren(Object parentElement) {
			DebugTreeItem item = (DebugTreeItem) parentElement;
			return item.children.toArray();
		}

		public Object getParent(Object element) {
			if (element instanceof DebugTreeItem) {
				return ((DebugTreeItem) element).parent;
			}
			return null;
		}

		public boolean hasChildren(Object element) {
			return getChildren(element).length > 0;
		}
	}

	public void createControl(final Composite parent) {
		interpretedModules = new ArrayList<IErlModule>();

		final Composite comp = new Composite(parent, SWT.NONE);
		setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		distributedDebugCheck = createCheckButton(comp,
				"Debug all connected nodes");

		final Group attachGroup = SWTUtil.createGroup(comp, "Auto Attach", 1,
				GridData.FILL_BOTH);
		attachGroup.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false,
				false));
		attachOnFirstCallCheck = createCheckButton(attachGroup, "First &call");
		attachOnBreakpointCheck = createCheckButton(attachGroup, "&Breakpoint");
		attachOnExitCheck = createCheckButton(attachGroup, "E&xit");

		distributedDebugCheck.addSelectionListener(fBasicSelectionListener);
		attachOnFirstCallCheck.addSelectionListener(fBasicSelectionListener);
		attachOnBreakpointCheck.addSelectionListener(fBasicSelectionListener);
		attachOnExitCheck.addSelectionListener(fBasicSelectionListener);

		final Group interpretedModulesGroup = new Group(comp, SWT.NONE);
		interpretedModulesGroup.setText("Interpreted modules");
		final GridData gd_interpretedModulesGroup = new GridData();
		interpretedModulesGroup.setLayoutData(gd_interpretedModulesGroup);
		interpretedModulesGroup.setLayout(new GridLayout());

		final Label anyModuleHavingLabel = new Label(interpretedModulesGroup,
				SWT.WRAP);
		anyModuleHavingLabel.setLayoutData(new GridData(279, SWT.DEFAULT));
		anyModuleHavingLabel
				.setText("Any module having breakpoints enabled will be dynamically added to the list.");

		checkboxTreeViewer = new CheckboxTreeViewer(interpretedModulesGroup,
				SWT.BORDER);
		checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				DebugTreeItem item = (DebugTreeItem) event.getElement();
				boolean checked = event.getChecked();

				if (checked) {
					if (item.item instanceof IErlModule) {
						if (!interpretedModules.contains(item.item)) {
							interpretedModules.add((IErlModule) item.item);
						}
					} else {
						for (DebugTreeItem c : item.children) {
							if (!interpretedModules.contains(c.item)) {
								interpretedModules.add((IErlModule) c.item);
							}
						}
					}
				} else {
					if (item.item instanceof IErlModule) {
						interpretedModules.remove(item.item);
					} else {
						for (DebugTreeItem c : item.children) {
							interpretedModules.remove(c.item);
						}
					}
				}

				checkboxTreeViewer.setSubtreeChecked(item, checked);
				// set gray state of the element's category subtree, all items
				// should not be grayed
				for (DebugTreeItem i : item.children) {
					checkboxTreeViewer.setGrayed(i, false);
				}
				checkboxTreeViewer.setGrayed(item, false);
				updateMenuCategoryCheckedState(item.parent);

			}

		});
		checkboxTreeViewer.setLabelProvider(new TreeLabelProvider());
		checkboxTreeViewer.setContentProvider(new TreeContentProvider());
		Tree tree = checkboxTreeViewer.getTree();
		final GridData gd_tree = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd_tree.minimumWidth = 250;
		gd_tree.minimumHeight = 120;
		gd_tree.widthHint = 256;
		gd_tree.heightHint = 220;
		tree.setLayoutData(gd_tree);
	}

	void updateMenuCategoryCheckedState(DebugTreeItem item) {
		if (item == null) {
			return;
		}
		if (item.isFullyChecked()) {
			checkboxTreeViewer.setParentsGrayed(item, false);
			checkboxTreeViewer.setChecked(item, true);
		} else if (item.isFullyUnchecked()) {
			checkboxTreeViewer.setParentsGrayed(item, false);
			checkboxTreeViewer.setChecked(item, false);
		} else {
			checkboxTreeViewer.setParentsGrayed(item, true);
			checkboxTreeViewer.setChecked(item, true);
		}
		updateMenuCategoryCheckedState(item.parent);
	}

	@SuppressWarnings("unchecked")
	public void setDefaults(final ILaunchConfigurationWorkingCopy config) {
		List<String> interpret;
		try {
			interpret = config.getAttribute(
					IErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
					new ArrayList<String>());
		} catch (CoreException e1) {
			interpret = new ArrayList<String>();
		}
		interpretedModules = new ArrayList<IErlModule>();
		for (String m : interpret) {
			String[] pm = m.split(":");
			IErlProject prj = ErlangCore.getModel().getErlangProject(pm[0]);
			try {
				IErlModule mod = prj.getModule(pm[1]);
				interpretedModules.add(mod);
			} catch (ErlModelException e) {
			}
		}

		int debugFlags;
		try {
			debugFlags = config.getAttribute(IErlLaunchAttributes.DEBUG_FLAGS,
					IErlDebugConstants.DEFAULT_DEBUG_FLAGS);
		} catch (final CoreException e) {
			debugFlags = IErlDebugConstants.DEFAULT_DEBUG_FLAGS;
		}
		setFlagCheckboxes(debugFlags);

		if (checkboxTreeViewer != null) {
			checkboxTreeViewer.setInput(config);
			checkboxTreeViewer.expandAll();
			DebugTreeItem root = ((TreeContentProvider) checkboxTreeViewer
					.getContentProvider()).root;
			setchecked(root, interpretedModules);
		} else {

		}
	}

	private void setchecked(DebugTreeItem item, List<IErlModule> list) {
		if (list.contains(item.item)) {
			checkboxTreeViewer.setChecked(item, true);
		}
		for (DebugTreeItem c : item.children) {
			setchecked(c, list);
			updateMenuCategoryCheckedState(item);
		}
	}

	public void initializeFrom(final ILaunchConfiguration config) {
		try {
			setDefaults(config.getWorkingCopy());
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	public void performApply(final ILaunchConfigurationWorkingCopy config) {
		config.setAttribute(IErlLaunchAttributes.DEBUG_FLAGS,
				getFlagChechboxes());

		List<String> r = new ArrayList<String>();
		for (IErlModule m : interpretedModules) {
			r.add(m.getProject().getName() + ":" + m.getName());
		}
		config.setAttribute(IErlLaunchAttributes.DEBUG_INTERPRET_MODULES, r);

	}

	public String getName() {
		return "Debug";
	}

	@Override
	public boolean isValid(final ILaunchConfiguration config) {
		return true;
	}

	private void setFlagCheckboxes(final int debugFlags) {
		attachOnFirstCallCheck
				.setSelection((debugFlags & IErlDebugConstants.ATTACH_ON_FIRST_CALL_FLAG) != 0);
		attachOnBreakpointCheck
				.setSelection((debugFlags & IErlDebugConstants.ATTACH_ON_BREAKPOINT_FLAG) != 0);
		attachOnExitCheck
				.setSelection((debugFlags & IErlDebugConstants.ATTACH_ON_EXIT_FLAG) != 0);
		distributedDebugCheck
				.setSelection((debugFlags & IErlDebugConstants.DISTRIBUTED_DEBUG_FLAG) != 0);
	}

	private int getFlagChechboxes() {
		int result = attachOnFirstCallCheck.getSelection() ? IErlDebugConstants.ATTACH_ON_FIRST_CALL_FLAG
				: 0;
		result += attachOnBreakpointCheck.getSelection() ? IErlDebugConstants.ATTACH_ON_BREAKPOINT_FLAG
				: 0;
		result += attachOnExitCheck.getSelection() ? IErlDebugConstants.ATTACH_ON_EXIT_FLAG
				: 0;
		result += distributedDebugCheck.getSelection() ? IErlDebugConstants.DISTRIBUTED_DEBUG_FLAG
				: 0;
		return result;
	}

	private final SelectionListener fBasicSelectionListener = new SelectionListener() {
		@SuppressWarnings("synthetic-access")
		public void widgetDefaultSelected(SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}

		@SuppressWarnings("synthetic-access")
		public void widgetSelected(SelectionEvent e) {
			updateLaunchConfigurationDialog();
		}
	};

}
