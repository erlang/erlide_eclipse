package org.erlide.debug.ui.views;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.debug.core.DebugEvent;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.IDebugEventSetListener;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.ui.AbstractDebugView;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.debug.ui.contexts.DebugContextEvent;
import org.eclipse.debug.ui.contexts.IDebugContextListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.erlide.core.erlang.IErlModule;
import org.erlide.core.util.ErlideUtil;
import org.erlide.runtime.backend.ErlangLaunchConfigurationDelegate;
import org.erlide.runtime.backend.ExecutionBackend;
import org.erlide.runtime.backend.IErlLaunchAttributes;
import org.erlide.runtime.debug.ErlDebugConstants;
import org.erlide.runtime.debug.ErlangDebugTarget;
import org.erlide.ui.launch.DebugTab;
import org.erlide.ui.launch.DebugTab.DebugTreeItem;
import org.erlide.ui.launch.DebugTab.TreeContentProvider;

/**
 * A view with a checkbox tree of interpreted modules checking/unchecking
 * interpretes/de-interpretes a module
 * 
 * @author jakob
 */
public class InterpretedModulesView extends AbstractDebugView implements
		IDebugEventSetListener, IDebugContextListener {

	CheckboxTreeViewer checkboxTreeViewer;
	private ErlangDebugTarget erlangDebugTarget;
	private boolean distributed;

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

	public void debugContextChanged(final DebugContextEvent event) {
		if ((event.getFlags() & DebugContextEvent.ACTIVATED) > 0) {
			contextActivated(event.getContext());
		}
	}

	private void contextActivated(final ISelection selection) {
		if (!isAvailable() || !isVisible()) {
			return;
		}
		final TreeContentProvider contentProvider = (TreeContentProvider) checkboxTreeViewer
				.getContentProvider();
		contentProvider.setRoot(new DebugTreeItem(null, null));
		final List<IErlModule> interpretedModules = new ArrayList<IErlModule>();
		if (selection instanceof IStructuredSelection) {
			final IStructuredSelection structuredSelection = (IStructuredSelection) selection;
			final Object o = structuredSelection.getFirstElement();
			if (o instanceof ErlangDebugTarget) {
				erlangDebugTarget = (ErlangDebugTarget) o;
				final ILaunchConfiguration launchConfiguration = erlangDebugTarget
						.getLaunch().getLaunchConfiguration();
				setViewerInput(launchConfiguration);
				checkboxTreeViewer.expandAll();
				try {
					final int debugFlags = launchConfiguration.getAttribute(
							IErlLaunchAttributes.DEBUG_FLAGS,
							ErlDebugConstants.DEFAULT_DEBUG_FLAGS);
					distributed = (debugFlags & ErlDebugConstants.DISTRIBUTED_DEBUG) != 0;
				} catch (final CoreException e1) {
					distributed = false;
				}
				DebugTab.addModules(erlangDebugTarget.getInterpretedModules(),
						interpretedModules);
			}
		}
		checkboxTreeViewer.refresh();
		final DebugTreeItem root = contentProvider.getRoot();
		if (root != null) {
			root.setChecked(checkboxTreeViewer, interpretedModules);
		}
		showViewer();

		// updateAction(VARIABLES_FIND_ELEMENT_ACTION);
		// updateAction(FIND_ACTION);
	}

	@SuppressWarnings("unchecked")
	private void setViewerInput(final ILaunchConfiguration launchConfiguration) {
		checkboxTreeViewer.setInput(launchConfiguration);
		if (launchConfiguration != null) {
			List<String> interpret;
			try {
				interpret = launchConfiguration.getAttribute(
						IErlLaunchAttributes.DEBUG_INTERPRET_MODULES,
						new ArrayList<String>());
			} catch (final CoreException e1) {
				interpret = new ArrayList<String>();
			}
			final ArrayList<IErlModule> interpretedModules = new ArrayList<IErlModule>(
					interpret.size());
			DebugTab.addModules(interpret, interpretedModules);
		}
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
			final DebugTreeItem root = ((TreeContentProvider) checkboxTreeViewer
					.getContentProvider()).getRoot();
			if (root == null) {
				return;
			}
			final Set<String> interpret = erlangDebugTarget
					.getInterpretedModules();
			final Set<IErlModule> interpretedModules = new HashSet<IErlModule>(
					interpret.size());
			DebugTab.addModules(interpret, interpretedModules);
			checkboxTreeViewer.getControl().getDisplay().syncExec(
					new Runnable() {
						public void run() {
							root.setChecked(checkboxTreeViewer,
									interpretedModules);
						}
					});
		}
	}

	@Override
	protected void configureToolBar(final IToolBarManager tbm) {
		// TODO Auto-generated method stub

	}

	@Override
	protected void createActions() {
		// TODO Auto-generated method stub

	}

	@Override
	protected Viewer createViewer(final Composite parent) {
		checkboxTreeViewer = new CheckboxTreeViewer(parent, SWT.BORDER);
		final ICheckStateListener checkStateListener = new ICheckStateListener() {
			public void checkStateChanged(final CheckStateChangedEvent event) {
				final DebugTab.DebugTreeItem dti = (DebugTreeItem) event
						.getElement();
				final boolean checked = event.getChecked();
				if (dti.getChildren().size() > 0) {
					checkboxTreeViewer.setSubtreeChecked(dti, checked);
				} else {
					final String module = dti.getItem().getName();
					final String moduleWoExtension = ErlideUtil
							.withoutExtension(module);
					final String project = dti.getItem().getErlProject()
							.getName();
					final boolean interpret = checked;
					final ExecutionBackend backend = erlangDebugTarget
							.getBackend();

					if (erlangDebugTarget.getInterpretedModules().contains(
							moduleWoExtension) != interpret) {
						ErlangLaunchConfigurationDelegate.interpret(backend,
								project, module, distributed, interpret);
					}
				}
			}

		};
		checkboxTreeViewer.addCheckStateListener(checkStateListener);
		checkboxTreeViewer.setLabelProvider(new DebugTab.TreeLabelProvider());
		checkboxTreeViewer
				.setContentProvider(new DebugTab.TreeContentProvider());
		DebugUITools.getDebugContextManager().addDebugContextListener(this);
		DebugPlugin.getDefault().addDebugEventListener(this);
		DebugPlugin.getDefault().addDebugEventListener(this);
		return checkboxTreeViewer;
	}

	@Override
	protected void fillContextMenu(final IMenuManager menu) {
		// TODO Auto-generated method stub
	}

	@Override
	protected String getHelpContextId() {
		// TODO Auto-generated method stub
		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractDebugView#becomesHidden()
	 */
	@Override
	protected void becomesHidden() {
		setViewerInput(null);
		super.becomesHidden();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.debug.ui.AbstractDebugView#becomesVisible()
	 */
	@Override
	protected void becomesVisible() {
		super.becomesVisible();
		final ISelection selection = DebugUITools.getDebugContextManager()
				.getContextService(getSite().getWorkbenchWindow())
				.getActiveContext();
		contextActivated(selection);
	}
}
