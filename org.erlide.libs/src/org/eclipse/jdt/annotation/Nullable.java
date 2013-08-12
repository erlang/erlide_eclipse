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
 * Qualifier for a type in a method signature or a local variable declaration.
 * The entity (return value, parameter, local variable) whose type has this
 * annotation is allowed to have the value <code>null</code> at runtime.
 * <p>
 * This has two consequences:
 * <ul>
 * <li>Binding a <code>null</code> value to the entity is legal.</li>
 * <li>Dereferencing the entity is unsafe, i.e., a
 * <code>NullPointerException</code> can occur at runtime.</li>
 * </ul>
 * </p>
 * 
 * @author stephan
 */
@Retention(RetentionPolicy.CLASS)
@Target({ METHOD, PARAMETER, LOCAL_VARIABLE })
public @interface Nullable {

}
