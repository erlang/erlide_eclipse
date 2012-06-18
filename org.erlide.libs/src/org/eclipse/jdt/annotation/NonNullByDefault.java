/*******************************************************************************
 * Copyright (c) 2011 Stephan Herrmann.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Stephan Herrmann - initial API and implementation 
 *******************************************************************************/
package org.eclipse.jdt.annotation;

import static java.lang.annotation.ElementType.*;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * <blockquote> This annotation is intended for use by the Eclipse Java Compiler
 * in order to support intra-procedural null analysis. Please see the original
 * <a href="http://bugs.eclipse.org/bugs/186342">Bug 186342- [compiler][null]
 * Using annotations for null checking</a> and the <a
 * href="http://wiki.eclipse.org/JDT_Core/Null_Analysis">Wiki page</a> for
 * status and availability of the implementation of these analyses.
 * </blockquote>
 * <p>
 * This annotation can be applied to a package or a type in order to define that
 * all contained entities for which a null annotation is otherwise lacking
 * should be considered as {@link NonNull @NonNull}.
 * <dl>
 * <dt>Interaction with inheritance</dt>
 * <dd>The rules regarding inheritance are applied <em>after</em> the applicable
 * default has been applied to all types lacking an annotation.</dd>
 * <dt>Nested defaults</dt>
 * <dd>If a <code>@NonNullByDefault</code> annotation is used within the scope
 * of a {@link NullableByDefault @NullableByDefault} annotation (or a project
 * wide default setting) the inner most annotation defines the default
 * applicable at any given position.</dd>
 * </dl>
 * Note that for applying an annotation to a package a file by the name
 * <code>package-info.java</code> is used.
 * 
 * @author stephan
 */
@Retention(RetentionPolicy.CLASS)
@Target({ PACKAGE, TYPE, METHOD, CONSTRUCTOR })
public @interface NonNullByDefault {
    boolean value() default true;
}
