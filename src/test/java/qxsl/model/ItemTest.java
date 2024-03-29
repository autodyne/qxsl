/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.model;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.*;

import static gaas.table.QxmlFactory.ITEM;

/**
 * {@link Item}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class ItemTest extends Assertions {
	private final Band band = new Band(7_000);
	private final Call call = new Call("JA1YYE");
	private final Mode mode = new Mode("SSB");
	private final Name name = new Name("pafelog");
	private final Note note = new Note("trivial");
	private final Time time = Time.now();

	@Test
	public void testEquals() {
		final var item1 = new Item();
		final var item2 = new Item();
		assertThat(item1).isEqualTo(item2);
		assertThat(item1.set(band).get(Qxsl.BAND)).isEqualTo(band);
		assertThat(item1.set(call).get(Qxsl.CALL)).isEqualTo(call);
		assertThat(item1.set(mode).get(Qxsl.MODE)).isEqualTo(mode);
		assertThat(item1.set(name).get(Qxsl.NAME)).isEqualTo(name);
		assertThat(item1.set(note).get(Qxsl.NOTE)).isEqualTo(note);
		assertThat(item1.set(time).get(Qxsl.TIME)).isEqualTo(time);
		assertThat(item1).isNotEqualTo(item2);
		assertThat(item2.set(band).get(Qxsl.BAND)).isEqualTo(band);
		assertThat(item2.set(call).get(Qxsl.CALL)).isEqualTo(call);
		assertThat(item2.set(mode).get(Qxsl.MODE)).isEqualTo(mode);
		assertThat(item2.set(name).get(Qxsl.NAME)).isEqualTo(name);
		assertThat(item2.set(note).get(Qxsl.NOTE)).isEqualTo(note);
		assertThat(item2.set(time).get(Qxsl.TIME)).isEqualTo(time);
		assertThat(item1).isEqualTo(item2);
	}

	@Test
	public void testType() {
		assertThat(new Item().name()).isEqualTo(ITEM);
	}
}
