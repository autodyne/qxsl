/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.field;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Qxsl;

import gaas.draft.*;

/**
 * {@link FieldManager}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class FieldManagerTest extends Assertions {
	private final FieldManager fields = new FieldManager();

	@Test
	public void testIterator() {
		assertThat(fields.iterator()).isNotNull();
		assertThat(fields.iterator()).hasNext();
	}

	@Test
	public void testGetFactory() {
		assertThat(fields.getFactory(Qxsl.BAND)).isInstanceOf(BandFactory.class);
		assertThat(fields.getFactory(Qxsl.CALL)).isInstanceOf(CallFactory.class);
		assertThat(fields.getFactory(Qxsl.CODE)).isInstanceOf(CodeFactory.class);
		assertThat(fields.getFactory(Qxsl.BAND)).isInstanceOf(BandFactory.class);
		assertThat(fields.getFactory(Qxsl.MODE)).isInstanceOf(ModeFactory.class);
		assertThat(fields.getFactory(Qxsl.NAME)).isInstanceOf(NameFactory.class);
		assertThat(fields.getFactory(Qxsl.NOTE)).isInstanceOf(NoteFactory.class);
		assertThat(fields.getFactory(Qxsl.RSTQ)).isInstanceOf(RSTQFactory.class);
		assertThat(fields.getFactory(Qxsl.TIME)).isInstanceOf(TimeFactory.class);
		assertThat(fields.getFactory(Qxsl.WATT)).isInstanceOf(WattFactory.class);
	}
}
