package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.EnumSet;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElement.AcceptFlags;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlElementVisitor;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.Test;

import com.google.common.collect.Lists;

public class IErlElementTest extends ErlModelTestBase {

    // boolean exists();
    @Test
    public void exists() throws Exception {
        final boolean exists = module.exists();
        module.getResource().delete(true, null);
        final boolean exists2 = module.exists();
        assertTrue(exists);
        assertFalse(exists2);
    }

    // IErlElement getAncestorOfKind(Kind kind);
    @Test
    public void getAncestorOfKind() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        final IErlElement ancestor = element.getAncestorOfKind(ErlElementKind.FUNCTION);
        final IErlElement ancestor2 = element.getAncestorOfKind(ErlElementKind.MODULE);
        final IErlElement ancestor3 = element.getAncestorOfKind(ErlElementKind.FOLDER);
        final IErlElement ancestor4 = element.getAncestorOfKind(ErlElementKind.PROJECT);
        final IErlElement ancestor5 = element.getAncestorOfKind(ErlElementKind.MODEL);
        final IErlElement ancestor6 = element.getAncestorOfKind(ErlElementKind.TYPESPEC);
        assertNotNull(ancestor);
        assertTrue(ancestor instanceof IErlFunction);
        assertEquals(ErlElementKind.FUNCTION, ancestor.getKind());
        assertEquals(element, ancestor);
        assertEquals(ErlElementKind.MODULE, ancestor2.getKind());
        assertEquals(ErlElementKind.FOLDER, ancestor3.getKind());
        assertEquals(ErlElementKind.PROJECT, ancestor4.getKind());
        assertEquals(ErlElementKind.MODEL, ancestor5.getKind());
        assertEquals(ancestor3, ancestor2.getAncestorOfKind(ErlElementKind.FOLDER));
        assertEquals(ancestor4, ancestor2.getAncestorOfKind(ErlElementKind.PROJECT));
        assertNull(ancestor6);
    }

    // IErlProject getProject();
    @Test
    public void getProject() throws Exception {
        assertEquals(project, ErlangEngine.getInstance().getModelUtilService()
                .getProject(module));
        assertEquals(project, ErlangEngine.getInstance().getModelUtilService()
                .getProject(project));
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(project, ErlangEngine.getInstance().getModelUtilService()
                .getProject(element));
    }

    // IErlModule getModule();
    @Test
    public void getModule() throws Exception {
        module.open(null);
        final IErlMember element = module.getElementAtLine(3);
        assertEquals(module,
                ErlangEngine.getInstance().getModelUtilService().getModule(element));
    }

    // IResource getCorrespondingResource();
    @Test
    public void getCorrespondingResource() throws Exception {
        project.open(null);
        final IProject workspaceProject = project.getWorkspaceProject();
        final IFolder srcFolder = workspaceProject.getFolder("src");
        final IFile file = srcFolder.getFile("xx.erl");
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule otpFile = model.findModuleFromProject(project, "file.erl", null,
                IErlElementLocator.Scope.PROJECT_ONLY);
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(file, module.getCorrespondingResource());
        assertNull(otpFile.getCorrespondingResource());
        assertNull(element.getCorrespondingResource());
    }

    // String getName();
    @Test
    public void getName() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        final IErlElement element2 = module.getElementAtLine(0);
        assertEquals("xx.erl", module.getName());
        assertEquals("testproject1", project.getName());
        assertEquals("f", element.getName());
        assertEquals("module", element2.getName());
    }

    // Kind getKind();
    @Test
    public void getKind() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        final IErlElement element2 = module.getElementAtLine(0);
        assertEquals(ErlElementKind.MODULE, module.getKind());
        assertEquals(ErlElementKind.PROJECT, project.getKind());
        assertEquals(ErlElementKind.FUNCTION, element.getKind());
        assertEquals(ErlElementKind.ATTRIBUTE, element2.getKind());
        assertEquals(ErlElementKind.MODEL, ErlangEngine.getInstance().getModel()
                .getKind());
    }

    // IErlModel getModel();
    @Test
    public void getModel() throws Exception {
        module.open(null);
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        assertEquals(model, ErlangEngine.getInstance().getModel());
    }

    // IParent getParent();
    @Test
    public void getParent() throws Exception {
        project.open(null);
        final IErlElement srcFolder = project.getChildNamed("src");
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(project, srcFolder.getParent());
        assertEquals(srcFolder, module.getParent());
        assertEquals(module, element.getParent());
    }

    // IResource getResource();
    @Test
    public void getResource() throws Exception {
        project.open(null);
        final IProject workspaceProject = project.getWorkspaceProject();
        final IFolder srcFolder = workspaceProject.getFolder("src");
        final IFile file = srcFolder.getFile("xx.erl");
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule otpFile = model.findModuleFromProject(project, "file.erl", null,
                IErlElementLocator.Scope.PROJECT_ONLY);
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(file, module.getResource());
        assertNull(otpFile.getResource());
        assertEquals(module.getResource(), element.getResource());
    }

    // boolean isReadOnly();
    // Empty method

    // boolean isStructureKnown() throws ErlModelException;
    @Test
    public void isStructureKnown() throws Exception {
        ((ErlProject) project).setSourceDirs(project.getProperties().getSourceDirs());
        // this sets structureKnown to false

        final boolean structureKnown = project.isStructureKnown();
        project.open(null);
        final boolean structureKnown2 = project.isStructureKnown();
        final boolean structureKnown3 = module.isStructureKnown();
        module.open(null);
        final boolean structureKnown4 = module.isStructureKnown();
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule otpFile = model.findModuleFromProject(project, "file.erl", null,
                IErlElementLocator.Scope.PROJECT_ONLY);
        final IErlExternal external = (IErlExternal) otpFile.getParent();
        final boolean structureKnown5 = external.isStructureKnown();
        final IErlModule module2 = ErlideTestUtils.createModule(project, "yy.erl",
                "-module(yy).\n% comment\n% same\nf(x) -> y.\n% last");
        final boolean structureKnown6 = module2.isStructureKnown();
        module2.open(null);
        final boolean structureKnown7 = module2.isStructureKnown();
        assertFalse(structureKnown);
        assertTrue(structureKnown2);
        assertFalse(structureKnown3);
        assertTrue(structureKnown4);
        assertTrue(structureKnown5);
        assertFalse(structureKnown6);
        assertTrue(structureKnown7);
    }

    // void resourceChanged(IResourceDelta delta);
    @Test
    public void resourceChanged() throws Exception {
        project.open(null);
        final boolean structureKnown = project.isStructureKnown();
        project.resourceChanged(new ErlideTestUtils.ResourceDeltaStub());
        final boolean structureKnown2 = project.isStructureKnown();
        assertTrue(structureKnown);
        assertFalse(structureKnown2);
    }

    // void accept(IErlElementVisitor visitor, EnumSet<AcceptFlags> flags,
    // IErlElement.Kind leafKind) throws ErlModelException;
    @Test
    public void accept() throws Exception {
        project.open(null);
        // ErlLogger.debug(project.getChildren().toString());
        module.open(null);
        final List<IErlElement> elements = Lists.newArrayList();
        final IErlElementVisitor visitor = new IErlElementVisitor() {

            @Override
            public boolean visit(final IErlElement element) throws ErlModelException {
                if (element instanceof IErlExternal) {
                    return false; // avoid digging through otp
                }
                final String name = element.getName();
                if (name.equals("ebin")) {
                    return false; // avoid possible beam-files
                } else if (name.startsWith(".")) {
                    return false; // avoid eclipse internals
                }
                elements.add(element);
                return true;
            }

        };
        final EnumSet<AcceptFlags> noneOf = EnumSet.noneOf(AcceptFlags.class);
        project.accept(visitor, noneOf, ErlElementKind.MODULE);
        final List<IErlElement> kindModuleElementsVisited = Lists.newArrayList(elements);
        elements.clear();
        project.accept(visitor, noneOf, ErlElementKind.FUNCTION);
        final List<IErlElement> kindFunctionElementsVisited = Lists
                .newArrayList(elements);
        elements.clear();
        project.accept(visitor, EnumSet.of(AcceptFlags.CHILDREN_FIRST),
                ErlElementKind.MODULE);
        final List<IErlElement> childrenFirst = Lists.newArrayList(elements);
        elements.clear();
        project.accept(visitor, EnumSet.of(AcceptFlags.LEAFS_ONLY), ErlElementKind.MODULE);
        final List<IErlElement> leafsOnly = Lists.newArrayList(elements);
        elements.clear();
        // assertEquals(4, kindModuleElementsVisited.size());
        assertEquals(project, kindModuleElementsVisited.get(0));
        // ErlLogger.debug(kindModuleElementsVisited.toString());
        assertEquals("include", kindModuleElementsVisited.get(1).getName());
        assertEquals("src", kindModuleElementsVisited.get(2).getName());
        assertEquals(module, kindModuleElementsVisited.get(3));
        assertEquals(7, kindFunctionElementsVisited.size());
        final int projectIndex = childrenFirst.indexOf(project);
        final int moduleIndex = childrenFirst.indexOf(module);
        assertTrue(moduleIndex < projectIndex);
        assertFalse(leafsOnly.contains(project));
        assertTrue(leafsOnly.contains(module));
    }

    // String getLabelString();
    // Should be removed

    // String getFilePath();
    @Test
    public void getFilePath() throws Exception {
        final String modulePath = module.getResource().getLocation().toString();
        // final String projectPath = project.getResource().getLocation()
        // .toString();
        // final String srcFolderPath = projectPath + "/src";
        final IErlElement parent = (IErlElement) module.getParent();
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(modulePath, module.getFilePath());
        assertNull(project.getFilePath());
        assertNull(parent.getFilePath());
        assertNull(element.getFilePath());
    }

    // void clearCaches();
    /**
     * @see org.erlide.engine.model.root.IErlElement#clearCaches()
     */
    // TODO check more than source dir cache
    @Test
    public void clearCaches() throws Exception {
    }

}
