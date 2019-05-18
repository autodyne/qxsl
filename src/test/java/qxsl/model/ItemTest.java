/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.model;

import org.junit.Test;
import qxsl.field.*;

import static qxsl.table.secret.QxmlFormat.ITEM;
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
	private final Band band = new Band(7_000);
	private final Call call = new Call("JA1YYE");
	private final Mode mode = new Mode("SSB");
	private final Name name = new Name("pafelog");
	private final Note note = new Note("trivial");
	private final Time time = new Time();
	@Test
	public void testType() {
		assertThat(new Item().type()).isEqualTo(ITEM);
	}
	@Test
	public void testEquals() {
		final Item item1 = new Item();
		final Item item2 = new Item();
		assertThat(item1).isEqualTo(item2);
		assertThat(item1.set(band).get(Band.class)).isEqualTo(band);
		assertThat(item1.set(call).get(Call.class)).isEqualTo(call);
		assertThat(item1.set(mode).get(Mode.class)).isEqualTo(mode);
		assertThat(item1.set(name).get(Name.class)).isEqualTo(name);
		assertThat(item1.set(note).get(Note.class)).isEqualTo(note);
		assertThat(item1.set(time).get(Time.class)).isEqualTo(time);
		assertThat(item1).isNotEqualTo(item2);
		assertThat(item2.set(band).get(Band.class)).isEqualTo(band);
		assertThat(item2.set(call).get(Call.class)).isEqualTo(call);
		assertThat(item2.set(mode).get(Mode.class)).isEqualTo(mode);
		assertThat(item2.set(name).get(Name.class)).isEqualTo(name);
		assertThat(item2.set(note).get(Note.class)).isEqualTo(note);
		assertThat(item2.set(time).get(Time.class)).isEqualTo(time);
		assertThat(item1).isEqualTo(item2);
	}
}
