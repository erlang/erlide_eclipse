package org.erlide.engine.util;

import org.eclipse.core.resources.IContainer;

public interface ContainerFilter {
    boolean accept(IContainer container);
}
