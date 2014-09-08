/*******************************************************************************
// * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     IBM Corporation - added J2SE 1.5 support
 *     Vlad Dumitrescu
 * *******************************************************************************/
package org.erlide.engine.model.erlang;

import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.ISourceUnit;

/**
 * Represents an entire Erlang compilation unit (<code>.erl</code> or
 * <code>.hrl</code> source file). Compilation unit elements need to be opened
 * before they can be navigated or manipulated. The children are of type
 * <code>IErlAttribute</code>, and <code>IErlFunction</code>, and appear in the
 * order in which they are declared in the source. If a <code>.erl</code> file
 * cannot be parsed, its structure remains unknown. Use
 * <code>IErlElement.isStructureKnown</code> to determine whether this is the
 * case.
 *
 * @noimplement This interface is not intended to be implemented by clients.
 */
public interface IErlModule extends IErlElement, IParent, IOpenable, ISourceUnit {

}
