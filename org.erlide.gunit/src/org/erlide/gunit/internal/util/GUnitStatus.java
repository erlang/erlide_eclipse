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
public class GUnitStatus implements IStatus {
	private String fStatusMessage;

	private int fSeverity;

	/**
	 * Creates a status set to OK (no message)
	 */
	public GUnitStatus() {
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
	public GUnitStatus(final int severity, final String message) {
		this.fStatusMessage = message;
		this.fSeverity = severity;
	}

	public static IStatus createError(final String message) {
		return new GUnitStatus(IStatus.ERROR, message);
	}

	public static IStatus createWarning(final String message) {
		return new GUnitStatus(IStatus.WARNING, message);
	}

	public static IStatus createInfo(final String message) {
		return new GUnitStatus(IStatus.INFO, message);
	}

	/**
	 * Returns if the status' severity is OK.
	 */
	public boolean isOK() {
		return this.fSeverity == IStatus.OK;
	}

	/**
	 * Returns if the status' severity is WARNING.
	 */
	public boolean isWarning() {
		return this.fSeverity == IStatus.WARNING;
	}

	/**
	 * Returns if the status' severity is INFO.
	 */
	public boolean isInfo() {
		return this.fSeverity == IStatus.INFO;
	}

	/**
	 * Returns if the status' severity is ERROR.
	 */
	public boolean isError() {
		return this.fSeverity == IStatus.ERROR;
	}

	/**
	 * @see IStatus#getMessage()
	 */
	public String getMessage() {
		return this.fStatusMessage;
	}

	/**
	 * Sets the status to ERROR.
	 * 
	 * @param errorMessage
	 *            the error message (can be empty, but not null)
	 */
	public void setError(final String errorMessage) {
		Assert.isNotNull(errorMessage);
		this.fStatusMessage = errorMessage;
		this.fSeverity = IStatus.ERROR;
	}

	/**
	 * Sets the status to WARNING.
	 * 
	 * @param warningMessage
	 *            the warning message (can be empty, but not null)
	 */
	public void setWarning(final String warningMessage) {
		Assert.isNotNull(warningMessage);
		this.fStatusMessage = warningMessage;
		this.fSeverity = IStatus.WARNING;
	}

	/**
	 * Sets the status to INFO.
	 * 
	 * @param infoMessage
	 *            the info message (can be empty, but not null)
	 */
	public void setInfo(final String infoMessage) {
		Assert.isNotNull(infoMessage);
		this.fStatusMessage = infoMessage;
		this.fSeverity = IStatus.INFO;
	}

	/**
	 * Sets the status to OK.
	 */
	public void setOK() {
		this.fStatusMessage = null;
		this.fSeverity = IStatus.OK;
	}

	/*
	 * @see IStatus#matches(int)
	 */
	public boolean matches(final int severityMask) {
		return (this.fSeverity & severityMask) != 0;
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
		return this.fSeverity;
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
		return this.fSeverity;
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
