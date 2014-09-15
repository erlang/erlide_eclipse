package org.erlide.engine.model.root;

import java.util.Collection;

import org.erlide.engine.model.IOpenable;
import org.erlide.engine.model.IParent;

public interface IErlLibrary extends IParent, IErlElement, IOpenable {

    Collection<IErlProject> getProjects();

    IErlProject getProject(String name);

}
