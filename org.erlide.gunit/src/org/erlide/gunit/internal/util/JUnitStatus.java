/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.erlide.gunit.internal.util;

import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IStatus;
import org.erlide.gunit.internal.ui.GUnitPlugin;

/**
 * An implemention of IStatus. TO DO: Why is it duplicated, it should leverage
 * the Status base class???
 */
public class JUnitStatus implements IStatus {
	private String fStatusMessage;
	private int fSeverity;

	/**
	 * Creates a status set to OK (no message)
	 */
	public JUnitStatus() {
		this(OK, null);
	}

	/**
	 * Creates a status .
	 * 
	 * @param severity
	 *            The status severity: ERROR, WARNING, INFO and OK.
	 * @param message
	 *            The message of the status. Applies only for ERROR, WARNING and
	 *            INFO.
	 */
	public JUnitStatus(int severity, String message) {
		fStatusMessage = message;
		fSeverity = severity;
	}

	public static IStatus createError(String message) {
		return new JUnitStatus(IStatus.ERROR, message);
	}

	public static IStatus createWarning(String message) {
		return new JUnitStatus(IStatus.WARNING, message);
	}

	public static IStatus createInfo(String message) {
		return new JUnitStatus(IStatus.INFO, message);
	}

	/**
	 * Returns if the status' severity is OK.
	 */
	public boolean isOK() {
		return fSeverity == IStatus.OK;
	}

	/**
	 * Returns if the status' severity is WARNING.
	 */
	public boolean isWarning() {
		return fSeverity == IStatus.WARNING;
	}

	/**
	 * Returns if the status' severity is INFO.
	 */
	public boolean isInfo() {
		return fSeverity == IStatus.INFO;
	}

	/**
	 * Returns if the status' severity is ERROR.
	 */
	public boolean isError() {
		return fSeverity == IStatus.ERROR;
	}

	/**
	 * @see IStatus#getMessage()
	 */
	public String getMessage() {
		return fStatusMessage;
	}

	/**
	 * Sets the status to ERROR.
	 * 
	 * @param errorMessage
	 *            the error message (can be empty, but not null)
	 */
	public void setError(String errorMessage) {
		Assert.isNotNull(errorMessage);
		fStatusMessage = errorMessage;
		fSeverity = IStatus.ERROR;
	}

	/**
	 * Sets the status to WARNING.
	 * 
	 * @param warningMessage
	 *            the warning message (can be empty, but not null)
	 */
	public void setWarning(String warningMessage) {
		Assert.isNotNull(warningMessage);
		fStatusMessage = warningMessage;
		fSeverity = IStatus.WARNING;
	}

	/**
	 * Sets the status to INFO.
	 * 
	 * @param infoMessage
	 *            the info message (can be empty, but not null)
	 */
	public void setInfo(String infoMessage) {
		Assert.isNotNull(infoMessage);
		fStatusMessage = infoMessage;
		fSeverity = IStatus.INFO;
	}

	/**
	 * Sets the status to OK.
	 */
	public void setOK() {
		fStatusMessage = null;
		fSeverity = IStatus.OK;
	}

	/*
	 * @see IStatus#matches(int)
	 */
	public boolean matches(int severityMask) {
		return (fSeverity & severityMask) != 0;
	}

	/**
	 * Returns always <code>false</code>.
	 * 
	 * @see IStatus#isMultiStatus()
	 */
	public boolean isMultiStatus() {
		return false;
	}

	/*
	 * @see IStatus#getSeverity()
	 */
	public int getSeverity() {
		return fSeverity;
	}

	/*
	 * @see IStatus#getPlugin()
	 */
	public String getPlugin() {
		return GUnitPlugin.PLUGIN_ID;
	}

	/**
	 * Returns always <code>null</code>.
	 * 
	 * @see IStatus#getException()
	 */
	public Throwable getException() {
		return null;
	}

	/**
	 * Returns always the error severity.
	 * 
	 * @see IStatus#getCode()
	 */
	public int getCode() {
		return fSeverity;
	}

	/**
	 * Returns always <code>null</code>.
	 * 
	 * @see IStatus#getChildren()
	 */
	public IStatus[] getChildren() {
		return new IStatus[0];
	}

}
