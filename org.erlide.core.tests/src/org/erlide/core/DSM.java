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
package org.erlide.core;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import jdepend.framework.JavaPackage;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class DSM {

    public abstract class Row {
        String name;
        List<Row> out;
        List<Row> in;
        Row parent;

        public void print() {
            System.out.print(name + ": ");
            for (final Row r : out) {
                System.out.print(r.name + ", ");
            }
            System.out.println();
        }
    }

    public class Pkg extends Row {
        public Pkg(final JavaPackage p) {
            parent = null;
            pkg = p;
            name = p.getName();
        }

        JavaPackage pkg;
    }

    public class Grp extends Row {
        List<Row> children;
    }

    final Map<JavaPackage, Pkg> dsm = Maps.newHashMap();

    @SuppressWarnings("unchecked")
    public DSM(final List<JavaPackage> input) {
        for (final JavaPackage p : input) {
            final Pkg r = new Pkg(p);
            dsm.put(p, r);
        }
        for (final JavaPackage p : input) {
            final Pkg r = dsm.get(p);
            r.in = makePkgList(dsm, p.getAfferents());
            r.out = makePkgList(dsm, p.getEfferents());
        }
    }

    private List<Row> makePkgList(final Map<JavaPackage, Pkg> map,
            final Collection<JavaPackage> jpkgs) {
        final List<Row> result = Lists.newArrayList();
        for (final JavaPackage p : jpkgs) {
            final Pkg ref = map.get(p);
            if (ref != null) {
                result.add(ref);
            }
        }
        return result;
    }

    public void print() {
        for (final Row r : dsm.values()) {
            r.print();
        }
    }

    public void partition() {

    }
}
