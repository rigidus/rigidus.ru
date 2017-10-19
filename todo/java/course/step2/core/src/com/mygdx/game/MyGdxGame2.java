package com.mygdx.game;

import com.badlogic.gdx.ApplicationAdapter;
import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.graphics.GL20;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.math.Vector2;

public class MyGdxGame extends ApplicationAdapter {
	SpriteBatch batch;
	Texture img;
	Vector2 position = new Vector2(10, 0); // *

	@Override
	public void create () {
		batch = new SpriteBatch();
		img = new Texture("badlogic.jpg");
	}

	@Override
	public void render () {
		Gdx.gl.glClearColor(0, 0, 0, 1);
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
		if (Gdx.input.isKeyPressed(Input.Keys.UP))    { position.y += 10; } // *
		if (Gdx.input.isKeyPressed(Input.Keys.DOWN))  { position.y -= 10; } // *
		if (Gdx.input.isKeyPressed(Input.Keys.LEFT))  { position.x -= 10; } // *
		if (Gdx.input.isKeyPressed(Input.Keys.RIGHT)) { position.x += 10; } // *
		batch.begin();
		batch.draw(img, position.x, position.y);
		batch.end();
	}

	@Override
	public void dispose () {
		batch.dispose();
		img.dispose();
	}
}
