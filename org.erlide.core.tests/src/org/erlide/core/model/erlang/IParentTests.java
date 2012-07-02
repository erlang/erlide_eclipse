package org.erlide.core.model.erlang;

import static org.junit.Assert.*;

import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.erlide.core.internal.model.erlang.ErlAttribute;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IErlElement.Kind;
import org.erlide.core.model.root.IErlFolder;
import org.erlide.core.model.root.IErlModel;
import org.junit.Test;

import com.google.common.collect.Lists;

public class IParentTests extends ErlModelTestBase {

    // List<IErlElement> getChildren() throws ErlModelException;
    @Test
    public void getChildren() throws Exception {
        final List<IErlElement> children = module.getChildren();
        module.open(null);
        final List<IErlElement> children2 = module.getChildren();
        assertEquals(0, children.size());
        assertEquals(3, children2.size());
        assertEquals(module, children2.get(0).getParent());
    }

    @Test(expected = UnsupportedOperationException.class)
    public void getChildren_unmodifiable() throws Exception {
        module.open(null);
        final List<IErlElement> children = module.getChildren();
        children.remove(0);
    }

    // int getChildCount();
    @Test
    public void getChildCount() throws Exception {
        final int childCount = module.getChildCount();
        module.open(null);
        final int childCount2 = module.getChildCount();
        project.open(null);
        final int childCount3 = project.getChildCount();
        assertEquals(0, childCount);
        assertEquals(3, childCount2);
        assertTrue(childCount3 >= 2);
    }

    // boolean hasChildren();
    @Test
    public void hasChildren() throws Exception {
        final boolean hasChildren = module.hasChildren();
        module.open(null);
        final boolean hasChildren2 = module.hasChildren();
        assertFalse(hasChildren);
        assertTrue(hasChildren2);
    }

    // List<IErlElement> getChildrenOfKind(Kind kind) throws ErlModelException;
    @Test
    public void getChildrenOfKind() throws Exception {
        module.open(null);
        final List<IErlElement> childrenOfKind = module
                .getChildrenOfKind(Kind.ATTRIBUTE);
        final List<IErlElement> childrenOfKind2 = module
                .getChildrenOfKind(Kind.FUNCTION);
        final List<IErlElement> childrenOfKind3 = module
                .getChildrenOfKind(Kind.TYPESPEC);
        assertEquals(2, childrenOfKind.size());
        assertEquals(1, childrenOfKind2.size());
        assertEquals(0, childrenOfKind3.size());
    }

    // boolean hasChildrenOfKind(Kind kind);
    @Test
    public void hasChildrenOfKind() throws Exception {
        module.open(null);
        final boolean hasChildrenOfKind = module
                .hasChildrenOfKind(Kind.ATTRIBUTE);
        final boolean hasChildrenOfKind2 = module
                .hasChildrenOfKind(Kind.FUNCTION);
        final boolean hasChildrenOfKind3 = module
                .hasChildrenOfKind(Kind.TYPESPEC);
        assertTrue(hasChildrenOfKind);
        assertTrue(hasChildrenOfKind2);
        assertFalse(hasChildrenOfKind3);
    }

    // IErlElement getChildNamed(String s);
    @Test
    public void getChildNamed() throws Exception {
        project.open(null);
        final String src = "src";
        final IErlElement childNamed = project.getChildNamed(src);
        final IErlElement childNamed2 = project.getChildNamed("SRC");
        final IErlElement childNamed3 = project.getChildNamed("noway");
        module.open(null);
        final IErlElement childNamed4 = module.getChildNamed("module");
        final IErlElement childNamed5 = module.getChildNamed("f");
        assertEquals(src, childNamed.getName());
        assertNull(childNamed2);
        assertNull(childNamed3);
        assertNotNull(childNamed4);
        assertTrue(childNamed5.getKind() == Kind.FUNCTION);
    }

    // IErlElement getChildWithResource(IResource rsrc);
    @Test
    public void getChildWithResource() throws Exception {
        final IProject workspaceProject = project.getWorkspaceProject();
        final IErlModel model = project.getModel();
        final IErlElement childWithResource = model
                .getChildWithResource(workspaceProject);
        final IResource resource = module.getResource();
        final IErlElement childWithResource2 = model
                .getChildWithResource(resource);
        final IErlFolder folder = (IErlFolder) project.getChildNamed("src");
        final IErlElement childWithResource3 = folder
                .getChildWithResource(resource);
        assertEquals(project, childWithResource);
        assertNull(childWithResource2);
        assertEquals(module, childWithResource3);
    }

    // void addChild(IErlElement child);
    @Test
    public void addChild() throws Exception {
        module.open(null);
        final int childCount = module.getChildCount();
        final String aname = "test_a";
        final IErlAttribute attribute = new ErlAttribute(module, aname, null,
                "test");
        module.addChild(attribute);
        final int childCount2 = module.getChildCount();
        final IErlElement childNamed = module.getChildNamed(aname);
        assertEquals(childCount + 1, childCount2);
        assertEquals(attribute, childNamed);
    }

    // public void setChildren(final Collection<? extends IErlElement>
    // children);
    @Test
    public void setChildren() throws Exception {
        module.open(null);
        final List<IErlElement> children = module.getChildren();
        final String aname = "test_a";
        final IErlAttribute attribute = new ErlAttribute(module, aname, null,
                "test");
        module.setChildren(Lists.newArrayList(attribute));
        final int childCount = module.getChildCount();
        final List<IErlElement> children2 = module.getChildren();
        final IErlElement element = children2.iterator().next();
        assertEquals(1, childCount);
        assertFalse(children2.equals(children));
        assertEquals(attribute, element);
    }

    // void removeChild(IErlElement e);
    @Test
    public void removeChild() throws Exception {
        module.open(null);
        final int childCount = module.getChildCount();
        final IErlElement element = module.getChildrenOfKind(Kind.ATTRIBUTE)
                .iterator().next();
        final IErlElement childNamed = module.getChildNamed(element.getName());
        module.removeChild(element);
        final int childCount2 = module.getChildCount();
        final IErlElement childNamed2 = module.getChildNamed(element.getName());
        assertEquals(childCount - 1, childCount2);
        assertNotNull(childNamed);
        assertNull(childNamed2);
    }

}
