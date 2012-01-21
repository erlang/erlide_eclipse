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
package org.erlide.utils;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The <code>LRUCache</code> is a hashtable that stores a finite number of
 * elements. When an attempt is made to add values to a full cache, the least
 * recently used values in the cache are discarded to make room for the new
 * values as necessary.
 * <p>
 * This implementation is NOT thread-safe. Synchronization wrappers would have
 * to be added to ensure atomic insertions and deletions from the cache.
 * 
 */
public class LRUCache<K, V> extends LinkedHashMap<K, V> {
    private static final long serialVersionUID = -5423610637179253987L;
    private final int max;

    public LRUCache(final int max_entries) {
        super(max_entries, .75F, true);
        max = max_entries;
    }

    @Override
    protected boolean removeEldestEntry(final Map.Entry<K, V> eldest) {
        return size() > max;
    }

    public Map<K, V> asSynchronized() {
        return Collections.synchronizedMap(this);
    }
}
