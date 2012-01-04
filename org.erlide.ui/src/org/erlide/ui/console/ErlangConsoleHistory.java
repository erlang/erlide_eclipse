/*******************************************************************************
 * Copyright (c) 2009 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.console;

import java.util.ArrayList;
import java.util.List;

public class ErlangConsoleHistory {
    private final List<String> history;
    private int navIndex;

    public ErlangConsoleHistory() {
        history = new ArrayList<String>();
    }

    public void addToHistory(final String in) {
        history.add(in);
        if (history.size() > 50) {
            history.remove(0);
        }
        navIndex = -1;
    }

    public List<String> getHistory() {
        return history;
    }

    public String get() {
        if (history.size() == 0) {
            return null;
        }
        return history.get(navIndex);
    }

    public int size() {
        return history.size();
    }

    void next() {
        if (navIndex == -1) {
            navIndex = 0;
        } else if (navIndex < history.size() - 1) {
            navIndex++;
        }
    }

    void prev() {
        if (navIndex == -1) {
            navIndex = history.size() - 1;
        } else if (navIndex > 0) {
            navIndex--;
        }
    }

    public void gotoLast() {
        navIndex = history.size() - 1;
    }

    public void gotoFirst() {
        navIndex = 0;
    }

}
