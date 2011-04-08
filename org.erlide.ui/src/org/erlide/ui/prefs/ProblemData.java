/*******************************************************************************
 * Copyright (c) 2008 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.prefs;

public class ProblemData {
    public String summary;
    public String reporter;
    public String description;
    public String platformLog;
    public String erlideLog;

    public ProblemData(final String title, final String body,
            final String contact, final String plog, final String elog) {
        summary = title;
        reporter = contact;
        description = body;
        platformLog = plog;
        erlideLog = elog;
    }

}
