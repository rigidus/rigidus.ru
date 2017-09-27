if ( ! Detector.webgl ) Detector.addGetWebGLMessage();

var renderer, scene, camera, stats;

var particles, uniforms;

var container, stats;
var camera, controls, scene, renderer;
var objects = [];

const PARTICLE_SIZE = 80;

var raycaster, intersects;
var mouse, INTERSECTED;

init();
animate();


function init() {

  container = document.createElement( 'div' );
  document.body.appendChild( container );

  camera = new THREE.PerspectiveCamera( 70, window.innerWidth / window.innerHeight, 1, 10000 );
  camera.position.z = 1000;

  controls = new THREE.TrackballControls( camera );
  controls.rotateSpeed = 1.0;
  controls.zoomSpeed = 1.2;
  controls.panSpeed = 0.8;
  controls.noZoom = false;
  controls.noPan = false;
  controls.staticMoving = true;
  controls.dynamicDampingFactor = 0.3;

  scene = new THREE.Scene();
  scene.background = new THREE.Color( 0xf0f0f0 );

  scene.add( new THREE.AmbientLight( 0x505050 ) );

  // light

  var light = new THREE.SpotLight( 0xffffff, 1.5 );
  light.position.set( 0, 500, 2000 );
  light.castShadow = true;

  light.shadow = new THREE.LightShadow( new THREE.PerspectiveCamera( 50, 1, 200, 10000 ) );
  light.shadow.bias = - 0.00022;

  light.shadow.mapSize.width = 2048;
  light.shadow.mapSize.height = 2048;

  scene.add( light );

  // overbox

  var geometry1 = new THREE.BoxGeometry( 2000, 2000, 2000, 10, 10, 10 );

  var vertices = geometry1.vertices;

  var positions = new Float32Array( vertices.length * 3 );
  var colors = new Float32Array( vertices.length * 3 );
  var sizes = new Float32Array( vertices.length );

  var vertex;
  var color = new THREE.Color();

  for ( var i = 0, l = vertices.length; i < l; i ++ ) {
    vertex = vertices[ i ];
    vertex.toArray( positions, i * 3 );
    color.setHSL( 0.01 + 0.1 * ( i / l ), 1.0, 0.5 );
    color.toArray( colors, i * 3 );
    sizes[ i ] = PARTICLE_SIZE * 1.5;
  }

  var geometry = new THREE.BufferGeometry();
  geometry.addAttribute( 'position', new THREE.BufferAttribute( positions, 3 ) );
  geometry.addAttribute( 'customColor', new THREE.BufferAttribute( colors, 3 ) );
  geometry.addAttribute( 'size', new THREE.BufferAttribute( sizes, 1 ) );

  var material = new THREE.ShaderMaterial( {
    uniforms: {
      color:   { value: new THREE.Color( 0xffffff ) },
      texture: { value: new THREE.TextureLoader().load( "/img/disc.png" ) }
    },
    vertexShader: document.getElementById( 'vertexshader' ).textContent,
    fragmentShader: document.getElementById( 'fragmentshader' ).textContent,
    alphaTest: 0.9
  } );

  particles = new THREE.Points( geometry, material );
  scene.add( particles );

  // random boxes

  var geometry = new THREE.BoxGeometry( 40, 40, 40 );

  for ( var i = 0; i < 20; i ++ ) {
    var object = new THREE.Mesh(
      geometry, new THREE.MeshLambertMaterial( { color: Math.random() * 0xffffff } ) );

    object.position.x = Math.random() * 1000 - 500;
    object.position.y = Math.random() * 600 - 300;
    object.position.z = Math.random() * 800 - 400;

    object.rotation.x = Math.random() * 2 * Math.PI;
    object.rotation.y = Math.random() * 2 * Math.PI;
    object.rotation.z = Math.random() * 2 * Math.PI;

    object.scale.x = Math.random() * 2 + 1;
    object.scale.y = Math.random() * 2 + 1;
    object.scale.z = Math.random() * 2 + 1;

    object.castShadow = true;
    object.receiveShadow = true;

    scene.add( object );

    objects.push( object );
  }

  // cylinder

  var geometry = new THREE.CylinderGeometry( 100, 100, 200, 32 );
  var material = new THREE.MeshLambertMaterial( {color: 0xffff00} );
   cylinder = new THREE.Mesh( geometry, material );
  scene.add( cylinder );

  // renderer

  renderer = new THREE.WebGLRenderer( { antialias: true } );
  renderer.setPixelRatio( window.devicePixelRatio );
  renderer.setSize( window.innerWidth, window.innerHeight );

  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFShadowMap;

  container.appendChild( renderer.domElement );


  raycaster = new THREE.Raycaster();
  mouse = new THREE.Vector2();

  // dra

  var dragControls = new THREE.DragControls( objects, camera, renderer.domElement );
  dragControls.addEventListener( 'dragstart', function ( event ) { controls.enabled = false; } );
  dragControls.addEventListener( 'dragend', function ( event ) { controls.enabled = true; } );

  var info = document.createElement( 'div' );
  info.style.position = 'absolute';
  info.style.top = '10px';
  info.style.width = '100%';
  info.style.textAlign = 'center';
  info.innerHTML = '<a href="http://threejs.org" target="_blank" rel="noopener">three.js</a> webgl - draggable cubes';
  container.appendChild( info );

  stats = new Stats();
  container.appendChild( stats.dom );

  //

  window.addEventListener( 'resize', onWindowResize, false );
  document.addEventListener( 'mousemove', onDocumentMouseMove, false );

}

function onDocumentMouseMove( event ) {
  event.preventDefault();
  mouse.x = ( event.clientX / window.innerWidth ) * 2 - 1;
  mouse.y = - ( event.clientY / window.innerHeight ) * 2 + 1;
}

function onWindowResize() {
  camera.aspect = window.innerWidth / window.innerHeight;
  camera.updateProjectionMatrix();
  renderer.setSize( window.innerWidth, window.innerHeight );
}

function animate() {
  requestAnimationFrame( animate );
  render();
  stats.update();
}

function render() {

  // cylinder.material.color.set(Math.random() * 0xffffff);
  // cylinder.rotation.y += 0.0005;


  // particles.rotation.x += 0.0005;
  // particles.rotation.y += 0.001;

  // var geometry = particles.geometry;
  // var attributes = geometry.attributes;

  // raycaster.setFromCamera( mouse, camera );
  // intersects = raycaster.intersectObject( particles );
  // if ( intersects.length > 0 ) {
  //   if ( INTERSECTED != intersects[ 0 ].index ) {
  //     // attributes.size.array[ INTERSECTED ] = PARTICLE_SIZE * 10;
  //     // INTERSECTED = intersects[ 0 ].index;
  //     // attributes.size.array[ INTERSECTED ] = PARTICLE_SIZE * 8.25;
  //     // attributes.size.needsUpdate = true;
  //   }
  // } else if ( INTERSECTED !== null ) {
  //   // attributes.size.array[ INTERSECTED ] = PARTICLE_SIZE * 10;
  //   // attributes.size.needsUpdate = true;
  //   // INTERSECTED = null;
  // }

  // var geo;

  controls.update();
  renderer.render( scene, camera );
}
