/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
package org.erlide.basicui.internal.util;

import org.eclipse.core.runtime.ListenerList;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IKeyBindingService;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;

public class ViewSiteDecorator extends PlatformObject implements IViewSite {

	private IViewSite fViewSite;

	protected ISelectionConverter fSelectionConverter;

	protected ISelectionProvider fSelectionProvider;

	private class SelectionProviderDecorator implements ISelectionProvider {

		private ISelectionProvider fProvider;

		private SelectionChangedListener fListener;

		public SelectionProviderDecorator(ISelectionProvider provider) {
			fProvider = provider;
		}

		public ISelection getSelection() {
			return fSelectionConverter.convert(fProvider.getSelection());
		}

		public void setSelection(ISelection selection) {
			throw new UnsupportedOperationException();
		}

		public void addSelectionChangedListener(
				ISelectionChangedListener listener) {
			if (fListener == null) {
				fListener = new SelectionChangedListener();
				fProvider.addSelectionChangedListener(fListener);
			}
			fListener.addListener(listener);
		}

		public void removeSelectionChangedListener(
				ISelectionChangedListener listener) {
			if (fListener == null) {
				return;
			}
			fListener.removeListener(listener);
			if (fListener.isEmpty()) {
				fProvider.removeSelectionChangedListener(fListener);
				fListener = null;
			}
		}
	}

	class SelectionChangedListener implements ISelectionChangedListener {

		ListenerList fListeners = new ListenerList();

		public void selectionChanged(SelectionChangedEvent event) {
			final ISelection selection = fSelectionConverter.convert(event
					.getSelection());
			final SelectionChangedEvent newEvent = new SelectionChangedEvent(
					fSelectionProvider, selection);
			final Object[] listeners = fListeners.getListeners();
			for (Object element : listeners) {
				((ISelectionChangedListener) element)
						.selectionChanged(newEvent);
			}
		}

		public void addListener(ISelectionChangedListener listener) {
			fListeners.add(listener);
		}

		public void removeListener(ISelectionChangedListener listener) {
			fListeners.remove(listener);
		}

		public boolean isEmpty() {
			return fListeners.isEmpty();
		}
	}

	public ViewSiteDecorator(IViewSite site, ISelectionConverter converter) {
		fViewSite = site;
		fSelectionConverter = converter;
		final ISelectionProvider provider = site.getSelectionProvider();
		if (provider != null) {
			fSelectionProvider = new SelectionProviderDecorator(provider);
		}
	}

	public IActionBars getActionBars() {
		return fViewSite.getActionBars();
	}

	public String getId() {
		return fViewSite.getId();
	}

	@Deprecated
	public IKeyBindingService getKeyBindingService() {
		return fViewSite.getKeyBindingService();
	}

	public String getPluginId() {
		return fViewSite.getPluginId();
	}

	public String getRegisteredName() {
		return fViewSite.getRegisteredName();
	}

	public String getSecondaryId() {
		return fViewSite.getSecondaryId();
	}

	public void registerContextMenu(String menuId, MenuManager menuManager,
			ISelectionProvider selectionProvider) {
		fViewSite.registerContextMenu(menuId, menuManager, selectionProvider);
	}

	public void registerContextMenu(MenuManager menuManager,
			ISelectionProvider selectionProvider) {
		fViewSite.registerContextMenu(menuManager, selectionProvider);
	}

	public IWorkbenchPage getPage() {
		return fViewSite.getPage();
	}

	public ISelectionProvider getSelectionProvider() {
		return fSelectionProvider;
	}

	public Shell getShell() {
		return fViewSite.getShell();
	}

	public IWorkbenchWindow getWorkbenchWindow() {
		return fViewSite.getWorkbenchWindow();
	}

	public void setSelectionProvider(ISelectionProvider provider) {
		throw new UnsupportedOperationException();
	}

	public IWorkbenchPart getPart() {
		// TODO Auto-generated method stub
		return null;
	}

	@SuppressWarnings("unchecked")
	public Object getService(Class api) {
		// TODO Auto-generated method stub
		return null;
	}

	@SuppressWarnings("unchecked")
	public boolean hasService(Class api) {
		// TODO Auto-generated method stub
		return false;
	}
}
