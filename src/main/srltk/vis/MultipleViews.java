package srltk.vis;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.image.TextureLoader;
import com.sun.j3d.utils.universe.*;
import com.sun.j3d.utils.picking.behaviors.*;
import com.sun.j3d.utils.picking.*;
import com.sun.j3d.utils.applet.MainFrame;


class CameraVie2w {

    protected static final PhysicalBody physBody = new PhysicalBody();
    protected static final PhysicalEnvironment physEnv =
                                            new PhysicalEnvironment();

    protected BranchGroup rootBG = null;
    protected TransformGroup vpTG = null;
    protected ViewPlatform viewPlatform = null;
    protected View view = null;
    protected Canvas3D canvas = null;

    public CameraVie2w() {

        GraphicsConfigTemplate3D gconfigTempl =
                                          new GraphicsConfigTemplate3D();
        GraphicsConfiguration gconfig =
                      GraphicsEnvironment.getLocalGraphicsEnvironment().
                                          getDefaultScreenDevice().
                                          getBestConfiguration( gconfigTempl );

        canvas = new Canvas3D( gconfig );

        view = new View();

        viewPlatform = new ViewPlatform();

        view.setPhysicalBody( physBody );
        view.setPhysicalEnvironment( physEnv );
        view.attachViewPlatform( viewPlatform );
        view.addCanvas3D( canvas );

        vpTG = new TransformGroup();
        vpTG.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        vpTG.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        vpTG.addChild( viewPlatform );

        rootBG = new BranchGroup();
        rootBG.setCapability( BranchGroup.ALLOW_DETACH );
        rootBG.addChild(vpTG);

    }

    public TransformGroup getViewPlatformTransformGroup() {
        return this.vpTG;
    }

    public BranchGroup getRootBG() {
        return this.rootBG;
    }

    public View getView() {
        return this.view;
    }

    public Canvas3D getCanvas3D() {
        return this.canvas;
    }

}


class UniverseBuilder {

    private Locale locale = null;
    private CameraVie2w riteCamera = null;
    private CameraVie2w mainCamera = null;

    public UniverseBuilder() {}

    public void addCameras() {

        VirtualUniverse universe = new VirtualUniverse();
        locale = new Locale( universe );

        riteCamera = new CameraVie2w();
        TransformGroup vpTG = riteCamera.getViewPlatformTransformGroup();
        Transform3D xform = new Transform3D();
        Vector3f vec = new Vector3f( 4.0f, 2.0f, 0.0f );
        xform.set( vec );
        Transform3D xform2 = new Transform3D();
        xform2.rotY( Math.PI / 2.0 );
        xform.mul( xform2 );
        vpTG.setTransform( xform );
        View view = riteCamera.getView();
        view.setProjectionPolicy( View.PARALLEL_PROJECTION );

        mainCamera = new CameraVie2w();
        vpTG = mainCamera.getViewPlatformTransformGroup();
        xform = new Transform3D();
        vec = new Vector3f( 0.0f, 0.0f, 4.0f );
        xform.set( vec );
        vpTG.setTransform( xform );
        view = mainCamera.getView();
        view.setProjectionPolicy( View.PARALLEL_PROJECTION );

 locale.addBranchGraph( riteCamera.getRootBG() );
        locale.addBranchGraph( mainCamera.getRootBG() );

    }

    public void addScene() {

        ModelScene scene = new ModelScene();
        BranchGroup sceneBG = scene.createSceneGraph();


        Canvas3D canvas = riteCamera.getCanvas3D();
        addMouseBehs( sceneBG, canvas );

        canvas = mainCamera.getCanvas3D();
        //addMouseBehs( sceneBG, canvas );

        sceneBG.compile();
        locale.addBranchGraph( sceneBG );

    }

    public void addMouseBehs(
                              BranchGroup rootBG,
                              Canvas3D canvas
                            ) {
        BoundingSphere bounds =
                   new BoundingSphere(new Point3d(0.0,0.0,0.0), 100.0);

        PickRotateBehavior rotbeh =
                     new PickRotateBehavior( rootBG, canvas, bounds);
        rootBG.addChild( rotbeh );

        PickZoomBehavior zoombeh =
                     new PickZoomBehavior( rootBG, canvas, bounds );
        rootBG.addChild( zoombeh );

        PickTranslateBehavior transbeh =
                     new PickTranslateBehavior( rootBG, canvas, bounds );
        rootBG.addChild( transbeh );
    }

    public CameraVie2w getRiteCamera() { return this.riteCamera; }
    public CameraVie2w getMainCamera() { return this.mainCamera; }

}

class ModelScene {

    public BranchGroup createSceneGraph() {

        BranchGroup objRoot = new BranchGroup();
        TransformGroup objTrans = new TransformGroup();
        objTrans.setCapability( TransformGroup.ALLOW_TRANSFORM_WRITE );
        objTrans.setCapability( TransformGroup.ALLOW_TRANSFORM_READ );
        objTrans.setCapability( javax.media.j3d.Node.ENABLE_PICK_REPORTING );

        objRoot.addChild( objTrans );

        objTrans.addChild( new ColorCube(0.4) );

        return objRoot;
    }
}


public class MultipleViews {

    public static void main(String args[]) {

        JFrame frame = new JFrame("MultipleViews");

        frame.addWindowListener( new WindowAdapter() {
            public void windowClosing( WindowEvent e ) {
                System.exit(0);
            }
            });

        UniverseBuilder uniBuilder = new UniverseBuilder();
        uniBuilder.addCameras();
        uniBuilder.addScene();

        JPanel ritePanel = new JPanel();
        ritePanel.setLayout( new BorderLayout() );
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout( new BorderLayout() );

        CameraVie2w riteCamera = uniBuilder.getRiteCamera();
        CameraVie2w mainCamera = uniBuilder.getMainCamera();

        Canvas3D riteCanvas = riteCamera.getCanvas3D();
        Canvas3D mainCanvas = mainCamera.getCanvas3D();
        riteCanvas.setSize( new Dimension( 300, 400 ) );
        mainCanvas.setSize( new Dimension( 300, 400 ) );

        ritePanel.add( riteCanvas, BorderLayout.CENTER );
        mainPanel.add( mainCanvas, BorderLayout.CENTER );

        JPanel mPanel = new JPanel();
        mPanel.setLayout( new GridLayout( 0, 2, 2, 2 ) );
        mPanel.add( mainPanel );
        mPanel.add( ritePanel );

        frame.getContentPane().add( mPanel );

        // frame.setSize( 600, 400 );
        frame.pack();
        frame.setVisible( true );

      }
}