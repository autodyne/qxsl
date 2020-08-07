/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.model;

import org.junit.jupiter.api.Test;
import qxsl.extra.field.*;

import static qxsl.extra.table.QxmlFormat.ITEM;

/**
 * {@link Item}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class ItemTest extends org.assertj.core.api.Assertions {
	private final Band band = new Band(7_000);
	private final Call call = new Call("JA1YYE");
	private final Mode mode = new Mode("SSB");
	private final Name name = new Name("pafelog");
	private final Note note = new Note("trivial");
	private final Time time = new Time();

	@Test
	public void testType() {
		assertThat(new Item().name()).isEqualTo(ITEM);
	}

	@Test
	public void testEquals() {
		final Item item1 = new Item();
		final Item item2 = new Item();
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
}
