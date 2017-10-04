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
	Texture img, brk;                                                   //
	Vector2 position = new Vector2(0, 0);
	Vector2 speed = new Vector2(0, 0);
	Vector2 maxPosition = new Vector2(0,0);
	Vector2 brickPosition = new Vector2(400, 200);                //

	@Override
	public void create () {
		batch = new SpriteBatch();
		img = new Texture("ball.png");
		brk = new Texture("brick.png"); //
		maxPosition.y = Gdx.graphics.getHeight() - img.getHeight();
		maxPosition.x = Gdx.graphics.getWidth() - img.getWidth();
	}

	@Override
	public void render () {
		Gdx.gl.glClearColor(0, 0, 0, 1);
		Gdx.gl.glClear(GL20.GL_COLOR_BUFFER_BIT);
		if (Gdx.input.isKeyPressed(Input.Keys.UP))    { speed.y += 1; }
		if (Gdx.input.isKeyPressed(Input.Keys.DOWN))  { speed.y -= 1; }
		if (Gdx.input.isKeyPressed(Input.Keys.LEFT))  { speed.x -= 1; }
		if (Gdx.input.isKeyPressed(Input.Keys.RIGHT)) { speed.x += 1; }
		if (speed.x != 0) { position.x = position.x + 2 * speed.x; }
		if (speed.y != 0) { position.y = position.y + 2 * speed.y; }
		if (position.x < 0) { speed.x = -speed.x; }             //
		if (position.y < 0) { speed.y = -speed.y; }             //
		if (position.x > maxPosition.x) { speed.x = -speed.x; } //
		if (position.y > maxPosition.y) { speed.y = -speed.y; } //
		// условие столкновения
		if (       (position.x                   < brickPosition.x + brk.getWidth()   )  // левая сторона img должна быть слева от правой стороны brk
				&& (position.x + img.getWidth()  > brickPosition.x                    )  // правая сторона img должна быть справа от левой стороны brk
				&& (position.y                   < brickPosition.y + brk.getHeight()  )  // верхняя сторона img должна быть сверху от нижней стороны brk
				&& (position.y + img.getHeight() > brickPosition.y                    )) // нижняя сторона img должна быть снизу от верхней стороны brk
		{
			// обнуляем скорость
			speed.x = 0;
			speed.y = 0;
		}
		batch.begin();
		batch.draw(img, position.x, position.y);
		batch.draw(brk, brickPosition.x, brickPosition.y);
		batch.end();
	}

	@Override
	public void dispose () {
		batch.dispose();
		img.dispose();
	}
}
