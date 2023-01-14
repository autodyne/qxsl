/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import java.nio.ByteBuffer;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.LineEvent;
import javax.sound.sampled.LineListener;
import javax.sound.sampled.LineUnavailableException;

import static java.lang.Integer.MAX_VALUE;
import static javax.sound.sampled.LineEvent.Type.STOP;

/**
 * 文字列をモールス符号の波形に変換します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2011/02/12
 */
public final class MorseTone {
	private static final double RISE = 0.1;
	private static final double FALL = 0.1;
	private final double unit;
	private final int rate;
	private final int tone;
	private final int wpms;

	/**
	 * 指定された周波数と速度の生成器を構築します。
	 *
	 *
	 * @param rate 標本化周波数
	 * @param tone 音程の周波数
	 * @param wpms 速度
	 */
	public MorseTone(int rate, int tone, int wpms) {
		this.rate = rate;
		this.tone = tone;
		this.wpms = wpms;
		this.unit = 60.0 / (wpms * 50.0);
	}

	/**
	 * モールス符号の配列のバイト数を計算します。
	 *
	 *
	 * @param code 符号
	 *
	 * @return バイト数
	 */
	private final int size(String code) {
		int size = 0;
		final var data = code.toCharArray();
		for(char ch: data) size += size(ch);
		return 4 * size;
	}

	/**
	 * モールス符号の配列のバイト数を計算します。
	 *
	 *
	 * @param code 符号
	 *
	 * @return バイト数
	 */
	private final int size(char code) {
		switch(code) {
			case ' ': return (int) (rate * unit * 3);
			case ';': return (int) (rate * unit * 1);
			case '_': return (int) (rate * unit * 4);
			case '.': return (int) (rate * unit * 2);
			default: throw new RuntimeException();
		}
	}

	/**
	 * 文字列をモールス符号の波形に変換します。
	 *
	 *
	 * @param text 再生する文字列
	 *
	 * @return 波形
	 */
	public final byte[] encode(String text) {
		final var code = new MorseCode().encode(text);
		final var buff = ByteBuffer.allocate(size(code));
		for(var ch: code.toCharArray()) encode(buff, ch);
		return buff.array();
	}

	/**
	 * 文字をモールス符号の波形に変換します。
	 *
	 *
	 * @param buff 波形の格納先
	 * @param code 再生する符号
	 *
	 * @return 波形の長さ
	 */
	private int encode(ByteBuffer buff, char code) {
		switch(code) {
			case ' ': return mute(buff, 3);
			case ';': return mute(buff, 1);
			case '_': return tone(buff, 3) + mute(buff, 1);
			case '.': return tone(buff, 1) + mute(buff, 1);
			default: throw new RuntimeException();
		}
	}

	/**
	 * 指定された時間の正弦波を生成します。
	 *
	 *
	 * @param buff 波形の格納先
	 * @param time 持続時間
	 *
	 * @return 波形の長さ
	 */
	private final int tone(ByteBuffer buff, int time) {
		final var tone = new SineTone(time);
		while(tone.hasNext()) buff.putInt(tone.next());
		return time;
	}

	/**
	 * 指定された時間の無音波を生成します。
	 *
	 *
	 * @param buff 波形の格納先
	 * @param time 持続時間
	 *
	 * @return 波形の長さ
	 */
	private final int mute(ByteBuffer buff, int time) {
		final int size = (int) (rate * unit * time);
		for(int t = 0; t < size; t++) buff.putInt(0);
		return time;
	}

	/**
	 * 指定された時間の正弦波を生成します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2021/09/20
	 */
	private final class SineTone {
		private final double step;
		private final int time;
		private final int rise;
		private final int fall;
		private int t;

		/**
		 * 指定された時間の正弦波を生成します。
		 *
		 *
		 * @param time 正弦波の長さ
		 */
		public SineTone(int time) {
			this.step = 2 * Math.PI * tone / rate;
			this.time = (int) (rate * unit * time);
			this.rise = (int) (rate * unit * RISE);
			this.fall = (int) (rate * unit * FALL);
		}

		public final boolean hasNext() {
			return t < this.time;
		}

		private final double peak() {
			final int r = this.time - t;
			if(t < rise) return t / (double) rise;
			if(r < fall) return r / (double) fall;
			return 1;
		}

		public final Integer next() {
			final double sin = Math.sin(step * t++);
			return (int) (sin * peak() * MAX_VALUE);
		}
	}

	/**
	 * この生成器に対応する音響の形式を返します。
	 *
	 *
	 * @return 音響の形式
	 */
	public final AudioFormat getAudioFormat() {
		return new AudioFormat(rate, 32, 1, true, true);
	}

	/**
	 * 指定された文字列の音響再生器を生成します。
	 *
	 *
	 * @param text 文字列
	 *
	 * @return 音響再生器
	 */
	public final Tone generate(String text) {
		return new Tone(encode(text));
	}

	/**
	 * モールス符号を音響再生します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2021/09/15
	 */
	public final class Tone {
		private final byte[] wave;

		/**
		 * 指定された波形を再生します。
		 *
		 *
		 * @param wave 波形
		 */
		private Tone(byte[] wave) {
			this.wave = wave;
		}

		/**
		 * 音響再生が終わるまで待機します。
		 *
		 *
		 * @throws LineUnavailableException 使用不可能の場合
		 */
		public void play() throws LineUnavailableException {
			synchronized(wave) {
				final var clip = AudioSystem.getClip();
				final var format = getAudioFormat();
				clip.open(format, wave, 0, wave.length);
				clip.addLineListener(new StopHandler());
				try {
					clip.start();
					wave.wait();
					clip.stop();
				} catch (InterruptedException ex) {
				} finally {
					clip.close();
				}
			}
		}

		/**
		 * 音響再生の終了を検知します。
		 *
		 *
		 * @author 無線部開発班
		 *
		 * @since 2021/09/15
		 */
		private final class StopHandler implements LineListener {
			@Override
			public final void update(LineEvent e) {
				if(e.getType() == STOP) {
					synchronized(wave) {
						wave.notify();
					}
				}
			}
		}
	}
}
