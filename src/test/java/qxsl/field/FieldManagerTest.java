/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.field;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import qxsl.draft.Qxsl;

import gaas.draft.*;

/**
 * {@link FieldManager}クラスの挙動を検査します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/25
 */
public final class FieldManagerTest extends Assertions {
	private final FieldManager fields = new FieldManager();

	@Test
	public void testGetFactory() {
		assertThat(fields.factory(Qxsl.BAND)).isInstanceOf(BandFactory.class);
		assertThat(fields.factory(Qxsl.CALL)).isInstanceOf(CallFactory.class);
		assertThat(fields.factory(Qxsl.CODE)).isInstanceOf(CodeFactory.class);
		assertThat(fields.factory(Qxsl.BAND)).isInstanceOf(BandFactory.class);
		assertThat(fields.factory(Qxsl.MODE)).isInstanceOf(ModeFactory.class);
		assertThat(fields.factory(Qxsl.NAME)).isInstanceOf(NameFactory.class);
		assertThat(fields.factory(Qxsl.NOTE)).isInstanceOf(NoteFactory.class);
		assertThat(fields.factory(Qxsl.RSTQ)).isInstanceOf(RSTQFactory.class);
		assertThat(fields.factory(Qxsl.TIME)).isInstanceOf(TimeFactory.class);
		assertThat(fields.factory(Qxsl.WATT)).isInstanceOf(WattFactory.class);
	}

	@Test
	public void testIterator() {
		assertThat(fields.iterator()).isNotNull();
		assertThat(fields.iterator()).hasNext();
	}
}
