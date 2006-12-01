/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.basicui.prefs;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.erlide.basiccore.ErtsInstall;
import org.erlide.basicui.ErlideBasicUIPlugin;

/**
 * Processes add/removed/changed VMs.
 */
public class ERTSsUpdater {

	// the VMs defined when this updated is instantiated
	/*  NOT USED
	private List fOriginalVMs;
	
	private String fDefaultVMInstallID;
	*/

	/**
	 * Contstructs a new VM updater to update VM install settings.
	 */
	public ERTSsUpdater() {
		//fOriginalVMs = new ArrayList(5);
		// ErtsInstall def = JavaRuntime.getDefaultVMInstall();

		// ErtsInstall[] vms = JavaRuntime.getVMInstalls();
		// for (int j = 0; j < vms.length; j++)
		// {
		// fOriginalVMs.add(vms[j]);
		// }
	}

	/**
	 * Updates VM settings and returns whether the update was successful.
	 * 
	 * @param jres
	 *            new installed JREs
	 * @param defaultJRE
	 *            new default VM
	 * @return whether the update was successful
	 */
	public boolean updateJRESettings(ErtsInstall[] jres, ErtsInstall defaultJRE) {

		// Create a VM definition container
		final List<ErtsInstall> vmContainer = new ArrayList<ErtsInstall>(5);

		// Set the default VM Id on the container
		// String defaultVMId = JavaRuntime.getCompositeIdFromVM(defaultJRE);
		// setDefaultVMInstallID(defaultVMId);

		for (ErtsInstall element : jres) {
			if (!vmContainer.contains(element)) {
				vmContainer.add(element);
			}
		}

		// Generate XML for the VM defs and save it as the new value of the VM
		// preference
		saveVMDefinitions(vmContainer);

		return true;
	}

	/* NOT USED
	private void setDefaultVMInstallID(String defaultVMId) {
		fDefaultVMInstallID = defaultVMId;
	}*/

	private void saveVMDefinitions(final List container) {
		final IRunnableWithProgress runnable = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor) {
				try {
					monitor.beginTask(ErtsMessages.ERTSsUpdater_0, 100);
					// String vmDefXML = asXML(container);
					monitor.worked(40);
					// JavaRuntime.getPreferences().setValue(JavaRuntime.PREF_VM_XML,
					// vmDefXML);
					monitor.worked(30);
					// JavaRuntime.savePreferences();
					monitor.worked(30);
					// } catch (IOException ioe)
					// {
					// ErlideBasicUIPlugin.log(ioe);
					// } catch (ParserConfigurationException e)
					// {
					// ErlideBasicUIPlugin.log(e);
					// } catch (TransformerException e)
					// {
					// ErlideBasicUIPlugin.log(e);
				} finally {
					monitor.done();
				}

			}

		};
		try {
			ErlideBasicUIPlugin.getDefault().getWorkbench()
					.getProgressService().busyCursorWhile(runnable);
		} catch (final InvocationTargetException e) {
			ErlideBasicUIPlugin.log(e);
		} catch (final InterruptedException e) {
			ErlideBasicUIPlugin.log(e);
		}
	}

	/*
	private String asXML(List container) {
		// TODO Auto-generated method stub
		return null;
	}
	*/
}
