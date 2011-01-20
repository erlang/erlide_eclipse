package org.erlide.core.erlang;

import java.util.Collection;

import org.eclipse.core.runtime.IProgressMonitor;

public interface IErlModuleInternal extends IParent {

    void removeChildren();

    void addComment(IErlComment c);

    Collection<IErlComment> getComments();

    boolean buildStructure(IProgressMonitor pm);

    void reconcileText(int offset, int removeLength, String newText,
            IProgressMonitor mon);

    void getScanner();

    void resetAndCacheScannerAndParser(String newText);

    void disposeScanner();

    boolean isStructureKnown();

    ErlToken getScannerTokenAt(int offset);

    String getPath();

    void setStructureKnown(boolean newStructureKnown);
}
