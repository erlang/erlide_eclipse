/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.util;

import org.eclipse.swt.program.Program;

public class SFProjectSupport {

    public static final String FEATURES_URL = "http://sourceforge.net/tracker/?func=add&group_id=58889&atid=489194";

    public static final String SUPPORT_URL = "http://sourceforge.net/tracker/?func=add&group_id=58889&atid=489192";

    public static final String BUGS_URL = "http://sourceforge.net/tracker/?func=add&group_id=58889&atid=489191";

    public static final String HOME_URL = "http://erlide.sourceforge.net";

    public static void openBugsReport() {
        Program.launch(BUGS_URL);

    }

    public static void openSupportRequest() {
        Program.launch(SUPPORT_URL);

    }

    public static void openFeatureRequest() {
        Program.launch(FEATURES_URL);

    }
}
