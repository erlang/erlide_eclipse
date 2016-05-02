/*******************************************************************************
 * Copyright (c) 2014, 2016 1C-Soft LLC and others. All rights reserved. This program and
 * the accompanying materials are made available under the terms of the Eclipse Public
 * License v1.0 which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vladimir Piskarev (1C) - initial API and implementation
 *******************************************************************************/
package org.erlide.engine.model.news;

/**
 * Represents a named property of an element.
 *
 * @param <T>
 *            the type of property values
 * @see ISourceElementInfo#get(Property)
 */
public final class Property<T> {
    private final String name;

    /**
     * Constructs a new property with the given name.
     *
     * @param name
     *            the name of the property (not <code>null</code>)
     */
    public Property(String name) {
        if (name == null) {
            throw new IllegalArgumentException();
        }
        this.name = name;
    }

    /**
     * Returns the name of the property.
     *
     * @return the property name (never <code>null</code>)
     */
    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return name;
    }
}
