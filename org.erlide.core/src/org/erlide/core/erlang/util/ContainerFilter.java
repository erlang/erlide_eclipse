package org.erlide.core.erlang.util;

import org.eclipse.core.resources.IContainer;

public interface ContainerFilter {
    boolean accept(IContainer container);
}
