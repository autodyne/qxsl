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
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;
import static qxsl.table.secret.QxmlFormat.ITEM;

/**
 * {@see Item}クラスのテスト用クラスです。
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
		assertThat(new Item().type(), is(ITEM));
	}
	@Test
	public void testEquals() {
		final Item item1 = new Item();
		final Item item2 = new Item();
		assertThat(item1, is(item2));
		assertThat(item1.set(band).get(Band.class), is(band));
		assertThat(item1.set(call).get(Call.class), is(call));
		assertThat(item1.set(mode).get(Mode.class), is(mode));
		assertThat(item1.set(name).get(Name.class), is(name));
		assertThat(item1.set(note).get(Note.class), is(note));
		assertThat(item1.set(time).get(Time.class), is(time));
		assertThat(item1, is(not(item2)));
		assertThat(item2.set(band).get(Band.class), is(band));
		assertThat(item2.set(call).get(Call.class), is(call));
		assertThat(item2.set(mode).get(Mode.class), is(mode));
		assertThat(item2.set(name).get(Name.class), is(name));
		assertThat(item2.set(note).get(Note.class), is(note));
		assertThat(item2.set(time).get(Time.class), is(time));
		assertThat(item1, is(item2));
	}
}
