/*******************************************************************************
 * Copyright (c) 2009 * and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     *
 *******************************************************************************/
package org.erlide.ui;

public enum ErlideImagePrefix {
    //@formatter:off
    T_OBJ("obj16"),
    T_OVR("ovr16"),
    T_WIZBAN("wizban"),
    T_ELCL("elcl16"),
    T_DLCL("dlcl16"),
    T_ETOOL("etool16"),
    T_EVIEW("eview16");
    //@formatter:on

    private String prefix;

    private ErlideImagePrefix(final String prefix) {
        this.prefix = prefix;
    }

    public String getPrefix() {
        return prefix;
    }
}
