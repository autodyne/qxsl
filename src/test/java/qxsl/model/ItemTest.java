/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import org.junit.Test;
import qxsl.extra.field.*;

import static qxsl.extra.table.QxmlFormat.ITEM;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Item}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/25
 *
 */
public final class ItemTest extends junit.framework.TestCase {
	private final Freq freq = new Freq(7_000);
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
		assertThat(item1.add(freq).get(Qxsl.FREQ)).isEqualTo(freq);
		assertThat(item1.add(call).get(Qxsl.CALL)).isEqualTo(call);
		assertThat(item1.add(mode).get(Qxsl.MODE)).isEqualTo(mode);
		assertThat(item1.add(name).get(Qxsl.NAME)).isEqualTo(name);
		assertThat(item1.add(note).get(Qxsl.NOTE)).isEqualTo(note);
		assertThat(item1.add(time).get(Qxsl.TIME)).isEqualTo(time);
		assertThat(item1).isNotEqualTo(item2);
		assertThat(item2.add(freq).get(Qxsl.FREQ)).isEqualTo(freq);
		assertThat(item2.add(call).get(Qxsl.CALL)).isEqualTo(call);
		assertThat(item2.add(mode).get(Qxsl.MODE)).isEqualTo(mode);
		assertThat(item2.add(name).get(Qxsl.NAME)).isEqualTo(name);
		assertThat(item2.add(note).get(Qxsl.NOTE)).isEqualTo(note);
		assertThat(item2.add(time).get(Qxsl.TIME)).isEqualTo(time);
		assertThat(item1).isEqualTo(item2);
	}
}
