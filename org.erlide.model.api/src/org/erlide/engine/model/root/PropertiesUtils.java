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
package org.erlide.engine.model.root;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public final class PropertiesUtils {

    @SuppressWarnings("unchecked")
    public static <U, T extends U> Collection<T> filter(
            final Collection<U> dependencies2, final Class<T> class1) {
        final List<T> result = new ArrayList<T>();
        for (final U oo : dependencies2) {
            if (oo.getClass().equals(class1)) {
                result.add((T) oo);
            }
        }
        return result;
    }

    private PropertiesUtils() {
    }
}
