package org.erlide.debug.ui.views;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.part.ViewPart;
import org.erlide.core.erlang.ErlModelException;
import org.erlide.core.erlang.ErlangCore;
import org.erlide.core.erlang.IErlElement;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.erlang.IErlProject;
import org.erlide.core.erlang.IErlModule.ModuleKind;
import org.erlide.runtime.backend.IErlLaunchAttributes;
import org.erlide.runtime.debug.ErlangDebugTarget;

public class InterpretedModulesView extends ViewPart {

	private CheckboxTreeViewer checkboxTreeViewer;
	private List<IErlModule> interpretedModules;

	private class TreeLabelProvider extends LabelProvider implements
			IDebugEventSetListener {
		@Override
		public String getText(final Object element) {
			if (element instanceof DebugTreeItem) {
				return ((DebugTreeItem) element).item.getName();
			}
			return super.getText(element);
		}

		@Override
		public Image getImage(final Object element) {
			return null;
		}

		public void handleDebugEvents(final DebugEvent[] events) {
			boolean changed = false;
			for (final DebugEvent debugEvent : events) {
				if (debugEvent.getKind() == DebugEvent.MODEL_SPECIFIC
						&& debugEvent.getDetail() == ErlangDebugTarget.INTERPRETED_MODULES_CHANGED) {
					changed = true;
					break;
				}
			}
			if (changed) {
				DebugPlugin.getDefault(); // FIXME
			}
		}
	}

	private class DebugTreeItem {
		private IErlElement item = null;
		private DebugTreeItem parent = null;
		private final List<DebugTreeItem> children = new ArrayList<DebugTreeItem>();

		private boolean isFullyChecked() {
			for (final DebugTreeItem i : children) {
				if (!interpretedModules.contains(i.item)) {
					return false;
				}
			}
			return true;
		}

		private boolean isFullyUnchecked() {
			for (final DebugTreeItem i : children) {
				if (interpretedModules.contains(i.item)) {
					return false;
				}
			}
			return true;
		}

	}

	private class TreeContentProvider implements IStructuredContentProvider,
			ITreeContentProvider {
		private String[] projects;
		private ILaunchConfiguration input;
		private DebugTreeItem root;

		public void inputChanged(final Viewer viewer, final Object oldInput,
				final Object newInput) {
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
				for (final String p : projects) {
					final IErlProject pj = ErlangCore.getModel()
							.getErlangProject(p);
					final DebugTreeItem m = new DebugTreeItem();
					m.item = pj;
					root.children.add(m);

					try {
						final List<IErlModule> ms = pj.getModules();
						for (final IErlModule mm : ms) {
							if (mm.getModuleKind() == ModuleKind.ERL) {
								final DebugTreeItem mi = new DebugTreeItem();
								mi.item = mm;
								mi.parent = m;
								m.children.add(mi);
							}
						}
					} catch (final ErlModelException e) {
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

		public Object[] getElements(final Object inputElement) {
			return root.children.toArray();
		}

		public Object[] getChildren(final Object parentElement) {
			final DebugTreeItem item = (DebugTreeItem) parentElement;
			return item.children.toArray();
		}

		public Object getParent(final Object element) {
			if (element instanceof DebugTreeItem) {
				return ((DebugTreeItem) element).parent;
			}
			return null;
		}

		public boolean hasChildren(final Object element) {
			return getChildren(element).length > 0;
		}
	}

	private void updateMenuCategoryCheckedState(final DebugTreeItem item) {
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
		} catch (final CoreException e1) {
			interpret = new ArrayList<String>();
		}
		interpretedModules = new ArrayList<IErlModule>();
		for (final String m : interpret) {
			final String[] pm = m.split(":");
			final IErlProject prj = ErlangCore.getModel().getErlangProject(
					pm[0]);
			try {
				final IErlModule mod = prj.getModule(pm[1]);
				if (mod != null) {
					interpretedModules.add(mod);
				}
			} catch (final ErlModelException e) {
			}
		}
		if (checkboxTreeViewer != null) {
			checkboxTreeViewer.setInput(config);
			checkboxTreeViewer.expandAll();
			final DebugTreeItem root = ((TreeContentProvider) checkboxTreeViewer
					.getContentProvider()).root;
			setChecked(root, interpretedModules);
		} else {

		}
	}

	private void setChecked(final DebugTreeItem item,
			final List<IErlModule> list) {
		if (list.contains(item.item)) {
			checkboxTreeViewer.setChecked(item, true);
		}
		for (final DebugTreeItem c : item.children) {
			setChecked(c, list);
			updateMenuCategoryCheckedState(item);
		}
	}

	@Override
	public void createPartControl(final Composite parent) {
		interpretedModules = new ArrayList<IErlModule>();

		final Composite comp = new Composite(parent, SWT.NONE);
		// setControl(comp);
		final GridLayout topLayout = new GridLayout();
		comp.setLayout(topLayout);

		// final Group interpretedModulesGroup = new Group(comp, SWT.NONE);
		// interpretedModulesGroup.setText("Interpreted modules");
		// final GridData gd_interpretedModulesGroup = new GridData();
		// interpretedModulesGroup.setLayoutData(gd_interpretedModulesGroup);
		// interpretedModulesGroup.setLayout(new GridLayout());
		//
		// final Label anyModuleHavingLabel = new Label(interpretedModulesGroup,
		// SWT.WRAP);
		// anyModuleHavingLabel.setLayoutData(new GridData(279, SWT.DEFAULT));
		// anyModuleHavingLabel
		// .setText("Any module having breakpoints enabled will be dynamically
		// added to the list.");

		checkboxTreeViewer = new CheckboxTreeViewer(comp, SWT.BORDER);
		checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				final DebugTreeItem item = (DebugTreeItem) event.getElement();
				final boolean checked = event.getChecked();

				if (checked) {
					if (item.item instanceof IErlModule) {
						if (!interpretedModules.contains(item.item)) {
							interpretedModules.add((IErlModule) item.item);
						}
					} else {
						for (final DebugTreeItem c : item.children) {
							if (!interpretedModules.contains(c.item)) {
								interpretedModules.add((IErlModule) c.item);
							}
						}
					}
				} else {
					if (item.item instanceof IErlModule) {
						interpretedModules.remove(item.item);
					} else {
						for (final DebugTreeItem c : item.children) {
							interpretedModules.remove(c.item);
						}
					}
				}

				checkboxTreeViewer.setSubtreeChecked(item, checked);
				// set gray state of the element's category subtree, all items
				// should not be grayed
				for (final DebugTreeItem i : item.children) {
					checkboxTreeViewer.setGrayed(i, false);
				}
				checkboxTreeViewer.setGrayed(item, false);
				updateMenuCategoryCheckedState(item.parent);

			}

		});
		checkboxTreeViewer.setLabelProvider(new TreeLabelProvider());
		checkboxTreeViewer.setContentProvider(new TreeContentProvider());
		final Tree tree = checkboxTreeViewer.getTree();
		final GridData gd_tree = new GridData(SWT.FILL, SWT.FILL, true, true);
		gd_tree.minimumWidth = 250;
		gd_tree.minimumHeight = 120;
		gd_tree.widthHint = 256;
		gd_tree.heightHint = 220;
		tree.setLayoutData(gd_tree);
	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

}
