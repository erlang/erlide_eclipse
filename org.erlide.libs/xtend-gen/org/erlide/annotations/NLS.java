/**
 * Copyright (c) 2014 Joerg Reichert
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Joerg Reichert
 */
package org.erlide.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import org.eclipse.xtend.lib.macro.Active;
import org.erlide.annotations.NLSProcessor;

@Target(ElementType.TYPE)
@Active(NLSProcessor.class)
public @interface NLS {
  public String propertyFileName();
}
