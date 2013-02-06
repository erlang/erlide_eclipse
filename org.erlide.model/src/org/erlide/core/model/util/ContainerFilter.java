package org.erlide.core.model.util;

import org.eclipse.core.resources.IContainer;

public interface ContainerFilter {
    boolean accept(IContainer container);
}
